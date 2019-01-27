import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
  Browser.element { 
    init = init
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

type GetResult
  = Failure
  | Success String
  | InProgress
  
init : () -> (Model, Cmd Msg)
init _ =
  (  Model "" "" InProgress
    , Cmd.none 
  )



-- UPDATE


type Msg
  = UpdateUsername String
  | UpdateApikey String
  | SubmitPair 
  | GotStat (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateUsername username ->
      ({ model | username = username }, Cmd.none )

    UpdateApikey apiKey ->
      ({ model | apiKey = apiKey }, Cmd.none )

    SubmitPair ->
      ({ model | username = String.reverse model.username }, Cmd.none )

    GotStat result ->
      case result of 
        Ok url ->
          ({model | curStat = Success url}, Cmd.none)

        Err _ ->
          ({model | curStat = Failure }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter username"
    , value model.username
    , onInput UpdateUsername ] []
    , br [] [] 
    , input [ type_ "password"
    , placeholder "Enter API key"
    , value model.apiKey
    , onInput UpdateApikey] []
    , div [] [
      button [ onClick SubmitPair ] [text "sUBMIT"]
    ] 
    , div [] [ text (model.username) ]
    , div [] [ text (model.apiKey)  ]
    ]

viewResult : Model -> Html Msg
viewResult model =
  case model.curStat of 
    Failure ->
      div []
        [ text "API request failed. "
          , button [] [ text "Yeah, this sucked."]
        ]

    Success url ->
      div []
        [
          text "look we succeed"
        ]

    InProgress ->
      div []
        [
          text "In progress"
        ]

-- HTTP

getPartyStatus : String -> String -> Cmd Msg
getPartyStatus username apiKey = 
  Http.request
    { method = "GET"
    , headers = createAuthHeader username apiKey
    , url = "https://habitica.com/api/v3/groups?type=party"
    , body = Http.emptyBody
    , expect = Http.expectString GotStat
    , timeout = Nothing
    , tracker = Nothing
    }

statDecoder : String -> Html Msg
statDecoder resulting =
  div []
    [
      h2 [] [text "String Vomit"]
      , text resulting
    ]

createAuthHeader : String -> String -> List Http.Header
createAuthHeader username apikey =
  [Http.header "x-api-user" username , Http.header "x-api-key" apikey ]
  


