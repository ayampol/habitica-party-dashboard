module Main exposing (GetResult(..), Model, Msg(..), createAuthHeader, getPartyStatus, init, main, statDecoder, subscriptions, update, view, viewResult)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, style, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, at, bool, dict, field, float, int, keyValuePairs, list, map2, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import List exposing (append)
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
    }


type QuestStatus
    = NoQuest
    | Quest QuestRecord


type alias QuestRecord =
    { progress : Progress
    , key : String
    , members : List ( String, Bool )
    }


type alias MemberStatus =
    { username : String
    , memProgress : MemberProgress
    }


type MemberProgress
    = MemBoss Float
    | MemCollect Int
    | Asleep


type GetResult
    = Failure
    | Success
    | Loading
    | Init


type Progress
    = Collect (List ( String, Int ))
    | Boss Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" Init NoQuest []
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateUsername String
    | UpdateApikey String
    | SubmitAll
    | GotAllMems (Result Http.Error ( QuestStatus, List MemberStatus ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdateApikey apiKey ->
            ( { model | apiKey = apiKey }, Cmd.none )

        SubmitAll ->
            ( { model | curStat = Loading }, Task.attempt GotAllMems (getAllMembers model) )

        GotAllMems result ->
            {--let
                a =
                    Debug.log "result" result
            in
                --}
            case result of
                Ok state ->
                    ( { model | curMembers = Tuple.second state, curQuest = Tuple.first state, curStat = Success }, Cmd.none )

                Err _ ->
                    ( { model | curStat = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textInput Nothing "Enter user ID" model.username UpdateUsername
        , br [] []
        , textInput (Just "password") "Enter API key" model.apiKey UpdateApikey
        , div []
            [ button [ onClick SubmitAll ] [ text "Get party status" ]
            ]
        , br [] []
        , viewResult model
        , br [] []
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


viewResult : Model -> Html Msg
viewResult model =
    case model.curStat of
        Failure ->
            div [ class "failure-text" ]
                [ text "API request failed. Either the server is down, or you forgot something."
                , br [] []
                , checkFailure model
                ]

        Success ->
            div []
                [ text "Here are your quest details: "
                , br [] []
                , viewQuest model
                ]

        Init ->
            div []
                [ text "Please enter your user ID and API key."
                ]

        Loading ->
            div []
                [ text "Fetching results..."
                ]


viewQuest : Model -> Html Msg
viewQuest model =
    case model.curQuest of
        NoQuest ->
            div []
                [ text "No quest active." ]

        Quest rec ->
            div []
                [ text "Quest is active."
                , br [] []
                , text ("Key is " ++ rec.key)
                , br [] []
                , viewProgress rec
                , div []
                    [ ul [ class "progress-list" ] (List.map viewMember model.curMembers)
                    ]
                , viewTotal model.curQuest <| computeTotalProgress model.curMembers
                ]


round2Float : Float -> Float
round2Float flot =
    toFloat (round (flot * 100)) / 100


viewMember : MemberStatus -> Html Msg
viewMember mem =
    case mem.memProgress of
        Asleep ->
            li []
                [ text (mem.username ++ " is taking a nap.")
                ]

        MemBoss progress ->
            li []
                [ --text ("Username: " ++ mem.username)
                  --, br [] []
                  --, text ("Up: " ++ String.fromFloat (round2Float progress))
                  text (mem.username ++ " will do " ++ String.fromFloat (round2Float progress) ++ " damage to the boss.")
                ]

        MemCollect progress ->
            li []
                [ --text ("Username: " ++ mem.username)
                  --, br [] []
                  --, text ("Items Collected: " ++ String.fromInt progress)
                  text (mem.username ++ " has collected " ++ String.fromInt progress ++ " items. ")
                ]


computeTotalProgress : List MemberStatus -> Float
computeTotalProgress memList =
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
                            "Total items collected is" ++ String.fromFloat tot

                        Boss _ ->
                            "Total damage pending is " ++ String.fromFloat tot
    in
    div [ class "progress-total" ]
        [ text totalText ]


viewProgress : QuestRecord -> Html Msg
viewProgress rec =
    case rec.progress of
        Collect items ->
            div []
                [ text "Collection quest in progress."
                , ul [] (List.map (\i -> li [] [ text (Tuple.first i ++ " " ++ String.fromInt (Tuple.second i)) ]) items)
                ]

        Boss hp ->
            text ("Bossfight. Boss HP : " ++ String.fromFloat (round2Float hp))



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


statDecoder : Decoder QuestStatus
statDecoder =
    succeed identity
        |> requiredAt [ "data", "quest", "active" ] bool
        |> Decode.andThen checkActive


checkActive : Bool -> Decoder QuestStatus
checkActive active =
    if not active then
        succeed NoQuest

    else
        succeed QuestRecord
            |> requiredAt [ "data", "quest", "progress" ] progDecoder
            |> requiredAt [ "data", "quest", "key" ] string
            |> requiredAt [ "data", "quest", "members" ] (keyValuePairs bool)
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
                Boss _ ->
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
    [ Http.header "x-api-user" username, Http.header "x-api-key" apikey ]
