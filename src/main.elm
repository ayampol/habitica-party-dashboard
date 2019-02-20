module Main exposing (GetResult(..), Model, Msg(..), createAuthHeader, getPartyStatus, init, main, statDecoder, subscriptions, update, view, viewResult)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, field, float, int, keyValuePairs, list, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)



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

    --  , questStat : QuestStatus
    }


type QuestStatus
    = NoQuest
    | Quest QuestRecord


type alias QuestRecord =
    { progress : Progress
    , key : String
    , members : List ( String, Bool )
    }


type GetResult
    = Failure
    | Success
    | InProgress


type Progress
    = Collect (List ( String, Int ))
    | Boss Float


type alias ProgRecord =
    { collect : Dict String Bool
    , boss : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" InProgress NoQuest
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateUsername String
    | UpdateApikey String
    | SubmitPair
    | GotStat (Result Http.Error QuestStatus)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdateApikey apiKey ->
            ( { model | apiKey = apiKey }, Cmd.none )

        SubmitPair ->
            ( model, getPartyStatus model.username model.apiKey )

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
        ]


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
                [ text "In progress"
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


createAuthHeader : String -> String -> List Http.Header
createAuthHeader username apikey =
    [ Http.header "x-api-user" username, Http.header "x-api-key" apikey ]



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
