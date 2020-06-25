module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Game
import Page.Top
import Router exposing (urlToRoute)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | TopPage Page.Top.Model
    | GamePage Page.Game.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( topModel, _ ) =
            Page.Top.init
    in
    Model key (TopPage topModel)
        |> goTo (urlToRoute url)


type Msg
    = TopMsg Page.Top.Msg
    | GameMsg Page.Game.Msg
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TopMsg topMsg ->
            case model.page of
                TopPage topModel ->
                    let
                        ( newTopModel, topCmd ) =
                            Page.Top.update topMsg topModel
                    in
                    ( { model | page = TopPage newTopModel }, Cmd.map TopMsg topCmd )

                _ ->
                    ( model, Cmd.none )

        GameMsg gameMsg ->
            case model.page of
                GamePage gameModel ->
                    let
                        ( newGameModel, gameCmd ) =
                            Page.Game.update gameMsg gameModel
                    in
                    ( { model | page = GamePage newGameModel }, Cmd.map GameMsg gameCmd )

                _ ->
                    ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            goTo (urlToRoute url) model


goTo : Maybe Router.Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Router.Top ->
            let
                ( topModel, topCmd ) =
                    Page.Top.init
            in
            ( { model | page = TopPage topModel }, Cmd.map TopMsg topCmd )

        Just Router.Game ->
            let
                ( gameModel, gameCmd ) =
                    Page.Game.init
            in
            ( { model | page = GamePage gameModel }, Cmd.map GameMsg gameCmd )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        TopPage topPage ->
            Page.Top.subscriptions topPage
                |> Sub.map TopMsg

        GamePage gamePage ->
            Page.Game.subscriptions gamePage
                |> Sub.map GameMsg

        NotFound ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div [ class "game_title" ] [ text "forest maze" ]
        , case model.page of
            NotFound ->
                pageNotFoudView

            TopPage topModel ->
                Page.Top.view topModel
                    |> Html.map TopMsg

            GamePage gameModel ->
                Page.Game.view gameModel
                    |> Html.map GameMsg
        ]
    }


pageNotFoudView : Html msg
pageNotFoudView =
    text "NotFound"
