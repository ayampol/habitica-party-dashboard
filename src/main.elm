module Main exposing (GetResult(..), Model, Msg(..), createAuthHeader, getPartyStatus, init, main, statDecoder, subscriptions, update, view, viewResult)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, field, float, int, list, null, oneOf, string, succeed)
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
    }


type alias QuestProgress =
    { -- There was a reason this was preferable. But I can't figure out how to decode to this.
      --progress : Progress
      prog : ProgRecord
    , active : Bool
    , key : String
    , members : Dict String Bool
    }



-- Use this instead of the one just above.
-- If quest is not active, we shouldn't save quest details.


type QuestStatus
    = NoQuest
    | Quest
        { progress : Progress
        , key : String
        , members : Dict String Bool
        }


type GetResult
    = Failure
    | Success String
    | InProgress


type Progress
    = Collect (Dict String Int)
    | Boss Float


type alias ProgRecord =
    { collect : Dict String Bool
    , boss : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" InProgress
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateUsername String
    | UpdateApikey String
    | SubmitPair
    | GotStat (Result Http.Error QuestProgress)


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
                    ( let
                        str =
                            printableQuestProg state
                      in
                      { model | curStat = Success str }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | curStat = Failure }, Cmd.none )



-- I would like this in the Model so it can actually be printed nicely
-- but how to init nested records?


printableQuestProg : QuestProgress -> String
printableQuestProg prog =
    if prog.active == True then
        "The quest is active \n"
            ++ " the key is "
            ++ prog.key
            ++ " there are "
            ++ String.fromInt (Dict.size prog.members)
            ++ " members\n"
            ++ checkProgress prog

    else
        "There is no quest active."


checkProgress : QuestProgress -> String
checkProgress q_prog =
    if Dict.isEmpty q_prog.prog.collect then
        "Boss battle. "
            ++ "HP = "
            ++ String.fromFloat q_prog.prog.boss

    else
        "We're collecting shit I guess"



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

        Success stat ->
            div []
                [ text ("look we succeed " ++ stat)
                ]

        InProgress ->
            div []
                [ text "In progress"
                ]



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



-- You have access to all the json in here.
-- Check active andThen parse the other fields as necessary.


statDecoder : Decoder QuestProgress
statDecoder =
    succeed QuestProgress
        |> requiredAt [ "data", "quest", "progress" ] progressDecoder
        |> requiredAt [ "data", "quest", "active" ] bool
        |> requiredAt [ "data", "quest", "key" ] string
        |> requiredAt [ "data", "quest", "members" ] (dict bool)



-- how to decode into union instead of record?
-- check one field then the other?


progressDecoder : Decoder ProgRecord
progressDecoder =
    succeed ProgRecord
        |> optional "collect" (dict bool) Dict.empty
        |> optional "hp" float 0.0



{--
 "progress":{  
            "collect":{  

            },
            "hp":100
         },
--}


progDecoder : Decoder Progress
progDecoder =
    -- check one field then the other
    -- what is hp when there is no boss fight?
    succeed Boss
        |> optional "hp" float 0.0


createAuthHeader : String -> String -> List Http.Header
createAuthHeader username apikey =
    [ Http.header "x-api-user" username, Http.header "x-api-key" apikey ]
