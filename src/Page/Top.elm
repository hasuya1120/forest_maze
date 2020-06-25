module Page.Top exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (attribute)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = AA


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AA ->
            ( model, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    a [ attribute "href" "/game"]
      [
        text "Start game"
      ]
