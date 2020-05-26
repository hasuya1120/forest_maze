module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import List.Extra


type alias Model =
    { board : Board }


type alias Board =
    List Point


type alias Point =
    { x : Int
    , y : Int
    , pointStatus : PointStatus
    }


type PointStatus
    = Wall
    | Road


init : ( Model, Cmd Msg )
init =
    ( { board = initializeBoard 8 }, Cmd.none )


initializeBoard : Int -> Board
initializeBoard numOfPoint =
    numOfPoint
        |> List.range 0
        |> initializePoints numOfPoint


checkPerimeter : Int -> Int -> Int -> PointStatus
checkPerimeter numOfPoint x y =
    if x == 0 || y == 0 || x == numOfPoint || y == numOfPoint then
        Wall

    else
        Road


initializePoint : Int -> Int -> Int -> Point
initializePoint numOfPoint x y =
    Point x y (checkPerimeter numOfPoint x y)


initializePoints : Int -> List Int -> Board
initializePoints numOfPoint width =
    List.Extra.lift2 (initializePoint numOfPoint) width width



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
