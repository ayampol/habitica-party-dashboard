module Main exposing (GetResult(..), Model, Msg(..), createAuthHeader, getPartyStatus, init, main, statDecoder, subscriptions, update, view, viewResult)

import Browser
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, placeholder, spellcheck, src, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, at, bool, dict, field, float, int, keyValuePairs, list, map, map2, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Json.Encode as JE exposing (Value)
import List exposing (append)
import Markdown as MD exposing (toHtml)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Svg exposing (animate, circle, svg)
import Svg.Attributes exposing (attributeName, begin, calcMode, cx, cy, dur, fill, fillOpacity, height, r, repeatCount, stroke, strokeOpacity, strokeWidth, style, values, width)
import Task exposing (Task, attempt)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { username : String
    , apiKey : String
    , curStat : GetResult
    , curQuest : QuestStatus
    , curMembers : List MemberStatus
    , allQuestDetails : Dict String String
    , chatHistory : List ChatEntry
    , socketState : State
    , socketSend : String
    , socketKey : String
    , socketError : Maybe String
    , socketLog : List String
    , socketWasLoaded : Bool
    , showSystem : Bool
    }


defaultUrl : String
defaultUrl =
    "ws://localhost:3000"


type alias ChatEntry =
    { uuid : String
    , text : String
    }


type QuestStatus
    = NoQuest
    | Quest QuestRecord


type alias QuestRecord =
    { progress : Progress
    , key : String
    , members : List ( String, Bool )
    , partyName : String
    }


type alias MemberStatus =
    { username : String
    , memProgress : MemberProgress
    }


type MemberProgress
    = MemBoss Float
    | MemCollect Int
    | Asleep


type alias ShowQuestStatus =
    Bool


type GetResult
    = Failure
    | Success ShowQuestStatus Chatlog
    | Loading
    | Init


type alias Chatlog =
    List String


type Progress
    = Collect (List ( String, Int ))
    | Boss Float (Maybe Float)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" Init NoQuest [] Dict.empty [] PortFunnels.initialState "Initial Message" "socket" Nothing [] False False
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateUsername String
    | UpdateApikey String
    | SubmitAll
    | GotQuestDetails (Result Http.Error (Dict String String))
    | GotChatHistory (Result Http.Error (List ChatEntry))
    | GotAllMems (Result Http.Error ( QuestStatus, List MemberStatus ))
      -- WebSocket Msgs
    | Connect
    | Close
    | Send
    | UpdateSend String
    | Process Value
    | PostChat
    | ChatSent (Result Http.Error ())
    | ToggleSystem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        connectToWs =
            WebSocket.makeOpenWithKey model.socketKey defaultUrl |> send model
    in
    case msg of
        UpdateUsername username ->
            { model | username = username } |> withNoCmd

        UpdateApikey apiKey ->
            { model | apiKey = apiKey } |> withNoCmd

        UpdateSend newSend ->
            { model | socketSend = newSend } |> withNoCmd

        Connect ->
            { model
                | socketLog =
                    ("Connecting to " ++ defaultUrl) :: model.socketLog
            }
                |> withCmd (WebSocket.makeOpenWithKey model.socketKey defaultUrl |> send model)

        ToggleSystem ->
            { model | showSystem = not model.showSystem } |> withNoCmd

        Send ->
            { model | socketSend = "" }
                |> withCmd (WebSocket.makeSend model.socketKey model.socketSend |> send model)

        Close ->
            { model | socketLog = "Closing" :: model.socketLog }
                |> withCmd (WebSocket.makeClose model.socketKey |> send model)

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.socketState model
            of
                Err error ->
                    { model | socketError = Just error } |> withNoCmd

                Ok res ->
                    res

        SubmitAll ->
            { model | curStat = Loading }
                |> withCmds [ Task.attempt GotAllMems (getAllMembers model), getQuestDetails, Task.attempt GotChatHistory (getChatHistory model.username model.apiKey), connectToWs ]

        GotQuestDetails result ->
            case result of
                Ok state ->
                    let
                        lowercasedkeys =
                            Dict.foldr smallKeys Dict.empty state
                    in
                    { model | allQuestDetails = lowercasedkeys } |> withNoCmd

                -- A missing quest details dictionary will show error messages for quest title/description
                Err _ ->
                    model |> withNoCmd

        GotAllMems result ->
            let
                a =
                    Debug.log "result" result
            in
            case result of
                Ok state ->
                    ( { model | curMembers = Tuple.second state, curQuest = Tuple.first state, curStat = Success True [ "" ] }, Cmd.none )

                Err _ ->
                    { model | curStat = Failure } |> withNoCmd

        GotChatHistory result ->
            --            let
            --                a =
            --                    Debug.log "result" result
            --            in
            case result of
                Ok state ->
                    { model | chatHistory = state } |> withNoCmd

                Err _ ->
                    { model | chatHistory = errorChatHistory } |> withNoCmd

        PostChat ->
            model |> withCmd (postChatMessage model.username model.apiKey model.socketSend)

        ChatSent result ->
            let
                a =
                    Debug.log "result" result
            in
            case result of
                Ok state ->
                    { model | socketSend = "" } |> withNoCmd

                Err _ ->
                    { model | chatHistory = errorChatSent model.chatHistory } |> withNoCmd


errorChatHistory : List ChatEntry
errorChatHistory =
    [ { uuid = "ERROR"
      , text = "Could not fetch history"
      }
    ]


errorChatSent : List ChatEntry -> List ChatEntry
errorChatSent entries =
    List.append
        [ { uuid = "ERROR"
          , text = "Could not send message"
          }
        ]
        entries


smallKeys : String -> String -> Dict String String -> Dict String String
smallKeys key value newDict =
    Dict.insert (String.toLower key) value newDict



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process



-- WEBSOCKET PORTS


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort


getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False


send : Model -> WebSocket.Message -> Cmd Msg
send model message =
    WebSocket.send (getCmdPort WebSocket.moduleName model) message


doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.socketWasLoaded && WebSocket.isLoaded model.socketState.websocket then
        { model
            | socketWasLoaded = True
        }

    else
        model


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl
                    | socketState = state
                    , socketError = Nothing
                }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            { model | socketLog = ("Received \"" ++ message ++ "\"") :: model.socketLog }
                |> withNoCmd

        WebSocket.ConnectedResponse r ->
            { model | socketLog = ("Connected: " ++ r.description) :: model.socketLog }
                |> withNoCmd

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | socketLog =
                    ("Closed, " ++ closedString code wasClean expected) :: model.socketLog
            }
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model | socketLog = WebSocket.errorToString error :: model.socketLog }
                |> withNoCmd

        -- Let's just ignore reconnected responses for now ...
        _ ->
            model |> withNoCmd


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "not expected"
           )



-- VIEW


view : Model -> Html Msg
view model =
    div [ swapLayout model ]
        [ viewResult model
        , br [] []
        , case model.curStat of
            Success showQuest _ ->
                viewChat model

            _ ->
                viewLoginForm model
        ]


swapLayout : Model -> Attribute Msg
swapLayout model =
    case model.curStat of
        Success showQuest _ ->
            class "master-success"

        _ ->
            class "master"


viewChat : Model -> Html Msg
viewChat model =
    let
        isConnected =
            WebSocket.isConnected model.socketKey model.socketState.websocket
    in
    div [ class "chat-master" ]
        [ div [ class "chat-converse" ]
            (logToHtml model.socketLog
                ++ chatHistoryToHtml
                    (cleanChatHistory
                        model.showSystem
                        model.chatHistory
                    )
            )
        , div []
            [ form [ onSubmit PostChat, class "chat-interact" ]
                [ textInput Nothing "Say something!" model.socketSend UpdateSend
                , button
                    [ class "chat-submit"
                    , type_ "submit"
                    , disabled (String.isEmpty model.socketSend)
                    ]
                    [ text " -> " ]
                ]
            , br [] []
            , if isConnected then
                button [ onClick Close ] [ text "Close" ]

              else
                button [ onClick Connect ] [ text "connect" ]
            , if model.showSystem then
                button [ onClick ToggleSystem ] [ text "Hide System " ]

              else
                button [ onClick ToggleSystem ] [ text "Show System" ]
            ]
        ]



-- Are we removing system messages?


cleanChatHistory : Bool -> List ChatEntry -> List ChatEntry
cleanChatHistory showSys entries =
    if showSys then
        entries

    else
        List.filter keepSystemMsg entries


keepSystemMsg : ChatEntry -> Bool
keepSystemMsg entry =
    if String.contains "system" entry.uuid then
        False

    else
        True


chatHistoryToHtml : List ChatEntry -> List (Html Msg)
chatHistoryToHtml entries =
    List.map chatEntryToHtml entries


chatEntryToHtml : ChatEntry -> Html Msg
chatEntryToHtml entry =
    p []
        [ text entry.uuid
        , text separator
        , MD.toHtml [] entry.text
        ]


separator : String
separator =
    " : "


logToHtml : List String -> List (Html Msg)
logToHtml log =
    List.map stringToBr log


stringToBr : String -> Html Msg
stringToBr str =
    p [] [ text str ]


viewLoginForm : Model -> Html Msg
viewLoginForm model =
    div [ spellcheck False ]
        [ form [ onSubmit SubmitAll ]
            [ textInput Nothing "Enter user ID" model.username UpdateUsername
            , br [] []
            , textInput (Just "password") "Enter API key" model.apiKey UpdateApikey
            , div []
                [ button [ type_ "submit" ] [ text " Log in " ]
                ]
            ]
        ]


textInput : Maybe String -> String -> String -> (String -> Msg) -> Html Msg
textInput whatType holdText modelValue updateMsg =
    input
        [ hasInputType whatType
        , placeholder holdText
        , value modelValue
        , onInput updateMsg
        ]
        []


hasInputType : Maybe String -> Attribute Msg
hasInputType whattype =
    case whattype of
        Just str ->
            type_ str

        Nothing ->
            type_ "text"


checkFailure : Model -> Html Msg
checkFailure model =
    let
        a =
            if String.isEmpty model.username then
                "Missing user ID."

            else
                ""

        b =
            if String.isEmpty model.apiKey then
                "Missing API key."

            else
                ""
    in
    text (a ++ " " ++ b)


checkShowQuest : Model -> Attribute Msg
checkShowQuest model =
    case model.curStat of
        Success True _ ->
            class "panel-show"

        _ ->
            class "panel"


viewResult : Model -> Html Msg
viewResult model =
    case model.curStat of
        Failure ->
            div [ class "failure-text" ]
                [ text "API request failed. There may be server issues or a field is wrong or missing."
                , br [] []
                , checkFailure model
                ]

        Success showQuest _ ->
            div [ class "success-text" ]
                [ div
                    [ checkShowQuest model ]
                    [ br [] []
                    , viewQuest model
                    ]
                ]

        Init ->
            div [ class "init-text" ]
                [ text "Please enter your user ID and API key."
                ]

        Loading ->
            div [ class "loading-results" ]
                [ loadSpinner
                , br [] []
                , text "Fetching results..."
                ]


viewQuest : Model -> Html Msg
viewQuest model =
    case model.curQuest of
        NoQuest ->
            div []
                [ text "No quest active." ]

        Quest rec ->
            div []
                [ keyToQuestHeader model.allQuestDetails rec.key
                , div [ class "quest-image" ]
                    [ img [ src (getImagePath rec.key) ] [] ]
                , br [] []
                , viewProgress rec
                , div [ class "quest-membersprog" ]
                    [ viewMembersTable model.curQuest
                        model.curMembers
                    ]
                ]


keyToQuestHeader : Dict String String -> String -> Html Msg
keyToQuestHeader allQuestDetails key =
    let
        questText =
            getWhatQuestDetail key "text" allQuestDetails

        questNotes =
            getWhatQuestDetail key "notes" allQuestDetails
    in
    div [ class "quest-details" ]
        [ p [ class "quest-title" ] [ text (maybeDetailToString questText "Quest title ") ]
        , br [] []
        , p [ class "quest-notes" ] [ text (maybeDetailToString questNotes "Notes ") ]
        ]


getWhatQuestDetail : String -> String -> Dict String String -> Maybe String
getWhatQuestDetail title detail allDetails =
    Dict.get ("quest" ++ String.toLower title ++ detail) allDetails


maybeDetailToString : Maybe String -> String -> String
maybeDetailToString dictResult name =
    case dictResult of
        Just result ->
            result

        Nothing ->
            name ++ " not found. "



-- This is suboptimal. But this will always get us the most up-to-date images.


getImagePath : String -> String
getImagePath key =
    "https://raw.githubusercontent.com/HabitRPG/habitica/develop/website/raw_sprites/spritesmith/quests/bosses/quest_" ++ key ++ ".png"


round2Float : Float -> Float
round2Float flot =
    toFloat (round (flot * 100)) / 100


viewMembersTable : QuestStatus -> List MemberStatus -> Html Msg
viewMembersTable quest members =
    let
        kind =
            case quest of
                NoQuest ->
                    "N/A"

                Quest questtype ->
                    case questtype.progress of
                        Boss _ _ ->
                            "Damage"

                        Collect _ ->
                            "Items"
    in
    table [ class "quest-memberstable" ]
        ([ thead [ class "members-tableheader" ]
            [ th [] [ text "Username" ]
            , th [] [ text kind ]
            ]
         ]
            ++ [ tbody []
                    (List.map viewMember members)
               ]
            ++ [ viewTotal quest <| computeTotalProgress members
               ]
        )


viewMember : MemberStatus -> Html Msg
viewMember mem =
    case mem.memProgress of
        Asleep ->
            tr []
                [ td []
                    [ text (mem.username ++ " is taking a nap.") ]
                , td [] []
                ]

        MemBoss progress ->
            tr
                []
                [ td [] [ text mem.username ]
                , td [] [ text <| String.fromFloat (round2Float progress) ]
                ]

        MemCollect progress ->
            tr
                []
                [ td [] [ text mem.username ]
                , td [] [ text <| String.fromInt progress ]
                ]


computeTotalProgress : List MemberStatus -> Float
computeTotalProgress memList =
    round2Float <|
        List.sum
            (List.map
                (\mem ->
                    case mem.memProgress of
                        MemBoss prog ->
                            round2Float prog

                        MemCollect prog ->
                            round2Float <| toFloat prog

                        Asleep ->
                            0
                )
                memList
            )


viewTotal : QuestStatus -> Float -> Html Msg
viewTotal questStat tot =
    let
        totalText =
            case questStat of
                NoQuest ->
                    ""

                Quest rec ->
                    case rec.progress of
                        Collect _ ->
                            "Total items collected "

                        Boss _ _ ->
                            "Total damage pending "
    in
    tfoot [ class "progress-total" ]
        [ tr []
            [ td [] [ text totalText ]
            , td [] [ text <| String.fromFloat tot ]
            ]
        ]


viewProgress : QuestRecord -> Html Msg
viewProgress rec =
    case rec.progress of
        Collect items ->
            div []
                [ text "Collection quest in progress."
                , ul [ class "collect__memprogress" ] (List.map (\i -> li [] [ text (Tuple.first i ++ " " ++ String.fromInt (Tuple.second i)) ]) items)
                ]

        Boss hp rage ->
            let
                healthText =
                    [ text ("Boss has " ++ String.fromFloat (round2Float hp) ++ " health left!") ]
            in
            div [ class "quest-summary" ]
                (healthText ++ deMaybeRage rage)


deMaybeRage : Maybe Float -> List (Html Msg)
deMaybeRage rage =
    case rage of
        Just flot ->
            [ br [] []
            , text ("Rage: " ++ String.fromFloat (round2Float flot))
            ]

        Nothing ->
            [ text "" ]


loadSpinner : Html Msg
loadSpinner =
    svg
        [ width "100", height "100" ]
        [ circle
            [ cx "50"
            , cy "50"
            , r "13.917525"
            , fill "#999999"
            , stroke "#909090"
            , strokeWidth "15"
            ]
            [ animate
                [ attributeName "r"
                , begin "0s"
                , dur "3s"
                , values "12;20;30;20;12;9"
                , calcMode "linear"
                , repeatCount "indefinite"
                ]
                []
            , animate
                [ attributeName "fill-opacity"
                , begin "0s"
                , dur "3s"
                , values "1;0.75;0.5;0.75;0.5;1"
                , calcMode "linear"
                , repeatCount "indefinite"
                ]
                []
            , animate
                [ attributeName "stroke-opacity"
                , begin "0s"
                , dur "3s"
                , values "1;0;0;0;1;1;1"
                , calcMode "linear"
                , repeatCount "indefinite"
                ]
                []
            , animate
                [ attributeName "stroke-width"
                , begin "0s"
                , dur "3s"
                , values "12;0;0;0;12;13"
                , calcMode "linear"
                , repeatCount "indefinite"
                ]
                []
            ]
        ]



-- HTTP


grabMembers : QuestStatus -> List String
grabMembers questStat =
    case questStat of
        NoQuest ->
            []

        Quest rec ->
            Tuple.first <| List.unzip rec.members


getAllMembers : Model -> Task Http.Error ( QuestStatus, List MemberStatus )
getAllMembers model =
    getPartyStatus model.username model.apiKey
        |> Task.andThen
            (\questStatus ->
                let
                    questStatusTask =
                        Task.succeed questStatus

                    allMembersTask =
                        Task.sequence (List.map (getMemberStatus questStatus model.username model.apiKey) (grabMembers questStatus))
                in
                Task.map2 Tuple.pair questStatusTask allMembersTask
            )


getPartyStatus : String -> String -> Task Http.Error QuestStatus
getPartyStatus username apiKey =
    Http.task
        { method = "GET"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/groups/party"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| statDecoder
        , timeout = Nothing
        }


getMemberStatus : QuestStatus -> String -> String -> String -> Task Http.Error MemberStatus
getMemberStatus currentQuest username apiKey id =
    Http.task
        { method = "GET"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/members/" ++ id
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| memDecoder (isBossQuest currentQuest)
        , timeout = Nothing
        }


getChatHistory : String -> String -> Task Http.Error (List ChatEntry)
getChatHistory username apiKey =
    Http.task
        { method = "GET"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/groups/party/chat"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| chatDecoder
        , timeout = Nothing
        }


getQuestDetails : Cmd Msg
getQuestDetails =
    Http.get
        { url = "https://raw.githubusercontent.com/HabitRPG/habitica/develop/website/common/locales/en/questsContent.json"
        , expect = Http.expectJson GotQuestDetails (dict string)
        }


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


postChatMessage : String -> String -> String -> Cmd Msg
postChatMessage username apiKey chatmsg =
    Http.request
        { method = "POST"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/groups/party/chat"
        , body = Http.jsonBody <| chatEncoder chatmsg
        , expect = Http.expectWhatever ChatSent
        , timeout = Nothing
        , tracker = Nothing
        }


chatEncoder : String -> JE.Value
chatEncoder str =
    JE.object [ ( "message", JE.string str ) ]


statDecoder : Decoder QuestStatus
statDecoder =
    succeed identity
        |> requiredAt [ "data", "quest", "active" ] bool
        |> Decode.andThen checkActive


chatDecoder : Decoder (List ChatEntry)
chatDecoder =
    at [ "data" ] <|
        Decode.list singleChatDecoder


singleChatDecoder : Decoder ChatEntry
singleChatDecoder =
    succeed identity
        |> required "uuid" string
        |> Decode.andThen getUserIfNotSystem


getUserIfNotSystem : String -> Decoder ChatEntry
getUserIfNotSystem str =
    let
        username =
            if str == "system" then
                hardcoded "system"

            else
                required "user" string
    in
    succeed ChatEntry
        |> username
        |> required "text" string


checkActive : Bool -> Decoder QuestStatus
checkActive active =
    if not active then
        succeed NoQuest

    else
        succeed QuestRecord
            |> requiredAt [ "data", "quest", "progress" ] progDecoder
            |> requiredAt [ "data", "quest", "key" ] string
            |> requiredAt [ "data", "quest", "members" ] (keyValuePairs bool)
            |> requiredAt [ "data", "name" ] string
            |> Decode.map Quest


progDecoder : Decoder Progress
progDecoder =
    succeed identity
        |> required "collect" (keyValuePairs int)
        |> Decode.andThen checkCollect


checkCollect : List ( String, Int ) -> Decoder Progress
checkCollect col =
    if List.isEmpty col then
        succeed Boss
            |> required "hp" float
            |> optional "rage" (Decode.map Just float) Nothing

    else
        succeed Collect
            |> required "collect" (keyValuePairs int)


isBossQuest : QuestStatus -> Bool
isBossQuest stat =
    case stat of
        NoQuest ->
            False

        Quest rec ->
            case rec.progress of
                Boss _ _ ->
                    True

                Collect _ ->
                    False


memDecoder : Bool -> Decoder MemberStatus
memDecoder bossQuest =
    let
        checkSleep : Bool -> Decoder MemberStatus
        checkSleep napping =
            if napping then
                succeed MemberStatus
                    |> requiredAt usernamePath string
                    |> hardcoded Asleep

            else
                awakeDecoder

        awakeDecoder : Decoder MemberStatus
        awakeDecoder =
            if bossQuest then
                memBossDecoder

            else
                memCollectDecoder
    in
    succeed identity
        |> requiredAt [ "data", "preferences", "sleep" ] bool
        |> Decode.andThen checkSleep


usernamePath : List String
usernamePath =
    [ "data", "auth", "local", "username" ]


memBossDecoder : Decoder MemberStatus
memBossDecoder =
    map2 MemberStatus
        (at usernamePath string)
        (Decode.map
            MemBoss
            (at [ "data", "party", "quest", "progress", "up" ] float)
        )


memCollectDecoder : Decoder MemberStatus
memCollectDecoder =
    map2 MemberStatus
        (at usernamePath string)
        (Decode.map
            MemCollect
            (at [ "data", "party", "quest", "progress", "collectedItems" ] int)
        )


createAuthHeader : String -> String -> List Http.Header
createAuthHeader username apikey =
    [ Http.header "x-api-user" username, Http.header "x-api-key" apikey, Http.header "x-client" "61557dda-101d-4f4d-bc59-4a48e9de22d1-partydash" ]
