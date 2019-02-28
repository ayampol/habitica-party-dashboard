module Main exposing (GetResult(..), Model, Msg(..), createAuthHeader, getPartyStatus, init, main, statDecoder, subscriptions, update, view, viewResult)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, field, float, int, keyValuePairs, list, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import List exposing (append)



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
    , boxen : String
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


type MemberStatus
    = Inactive String
    | Active MemberProgress


type alias MemberProgress =
    { up : Float
    , down : Float
    , collected : Int
    , username : String
    }


type GetResult
    = Failure
    | Success
    | InProgress


type Progress
    = Collect (List ( String, Int ))
    | Boss Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" InProgress NoQuest []
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateUsername String
    | UpdateApikey String
    | UpdateBoxen String
    | SubmitPair
    | SubmitMem String
    | GotStat (Result Http.Error QuestStatus)
    | GotMem (Result Http.Error MemberStatus)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdateApikey apiKey ->
            ( { model | apiKey = apiKey }, Cmd.none )

        UpdateBoxen boxen ->
            ( { model | boxen = boxen }, Cmd.none )

        SubmitPair ->
            ( model, getPartyStatus model.username model.apiKey )

        SubmitMem id ->
            ( model, Cmd.batch [ getMemberStatus model model.username model.apiKey id, getPartyStatus model.username model.apiKey ] )

        GotStat result ->
            let
                a =
                    Debug.log "result" result
            in
            case result of
                Ok state ->
                    ( { model | curStat = Success, curQuest = state }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | curStat = Failure }, Cmd.none )

        GotMem result ->
            let
                a =
                    Debug.log "result" result
            in
            case result of
                Ok state ->
                    ( { model | curMembers = List.append model.curMembers [ state ] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Enter username"
            , value model.username
            , onInput UpdateUsername
            ]
            []
        , br [] []
        , input
            [ type_ "password"
            , placeholder "Enter API key"
            , value model.apiKey
            , onInput UpdateApikey
            ]
            []
        , div []
            [ button [ onClick SubmitPair ] [ text "sUBMIT" ]
            ]
        , div [] [ text model.username ]
        , div [] [ text model.apiKey ]
        , br [] []
        , viewResult model
        , br [] []
        , input
            [ placeholder "Enter a member ID"
            , value model.boxen
            , onInput UpdateBoxen
            ]
            []
        , div []
            [ button [ onClick (SubmitMem model.boxen) ]
                [ text "Get one member" ]
            ]
        ]



{--
textInput : String -> String -> String -> Msg -> Html Msg
textInput whattype holdtext modelvalue updatemsg =
    input
        [ type_ whattype
        , placeholder holdtext
        , value modelvalue
        , onInput updatemsg
        ]
        []
        --}


viewResult : Model -> Html Msg
viewResult model =
    case model.curStat of
        Failure ->
            div []
                [ text "API request failed. "
                , button [] [ text "Yeah, this sucked." ]
                ]

        Success ->
            div []
                [ text "look we succeed "
                , br [] []
                , viewQuest model
                ]

        InProgress ->
            div []
                [ text "Please enter your username and API key."
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
                , ul [] (List.map viewMemberUp model.curMembers)
                ]


viewMemberUp : MemberStatus -> Html Msg
viewMemberUp mem =
    case mem of
        Inactive str ->
            div []
                [ text (str ++ " is taking a nap.")
                ]

        Active progress ->
            div []
                [ text ("Username: " ++ progress.username)
                , br [] []
                , text ("Up: " ++ String.fromFloat progress.up)
                ]


viewMemberCollected : MemberProgress -> Html Msg
viewMemberCollected mem =
    div []
        [ text ("Username: " ++ mem.username)
        , br [] []
        , text ("Up: " ++ String.fromFloat mem.up)
        ]


viewProgress : QuestRecord -> Html Msg
viewProgress rec =
    case rec.progress of
        Collect items ->
            div []
                [ text "Collection quest in progress."
                , ul [] (List.map (\i -> li [] [ text (Tuple.first i ++ " " ++ String.fromInt (Tuple.second i)) ]) items)
                ]

        Boss hp ->
            text ("Bossfight. Boss HP : " ++ String.fromFloat hp)



-- HTTP


getPartyStatus : String -> String -> Cmd Msg
getPartyStatus username apiKey =
    Http.request
        { method = "GET"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/groups/party"
        , body = Http.emptyBody
        , expect = Http.expectJson GotStat statDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getMemberStatus : Model -> String -> String -> String -> Cmd Msg
getMemberStatus model username apiKey id =
    Http.request
        { method = "GET"
        , headers = createAuthHeader username apiKey
        , url = "https://habitica.com/api/v3/members/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson GotMem memDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


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


sleepCheck : Bool -> Decoder MemberStatus
sleepCheck napping =
    if napping then
        succeed identity
            |> requiredAt [ "data", "auth", "local", "username" ] string
            |> Decode.map Inactive

    else
        memDecoder


memDecoder : Decoder MemberStatus
memDecoder =
    succeed MemberProgress
        |> requiredAt [ "data", "party", "quest", "progress", "up" ] float
        |> requiredAt [ "data", "party", "quest", "progress", "down" ] float
        |> requiredAt [ "data", "party", "quest", "progress", "collectedItems" ] int
        |> requiredAt [ "data", "auth", "local", "username" ] string
        |> Decode.map Active


createAuthHeader : String -> String -> List Http.Header
createAuthHeader username apikey =
    [ Http.header "x-api-user" username, Http.header "x-api-key" apikey ]



{--
for(id in party.quest.members) {
          var member = Utils.fetch("https://habitica.com/api/v3/members/" + id, "get");
            if(party.quest.members[id]) {
                        var memberProgress = isBossQuest ? member.party.quest.progress.up : member.party.quest.progress.collectedItems;
                        totalProgress += memberProgress;
                                
           if(member.preferences.sleep)
                        sleepProgress += memberProgress;
                        }
           }
     --}
-- how to decode into union instead of record?
-- check one field then the other?
--progressDecoder : Decoder ProgRecord
--progressDecoder =
--    succeed ProgRecord
--        |> optional "collect" (dict bool) Dict.empty
--        |> optional "hp" float 0.0
{--
 "progress":{  
            "collect":{  

            },
            "hp":100
         },
--}
