module Page.Game exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)


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
    text "game"
