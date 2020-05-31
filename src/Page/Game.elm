module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

-- Write the end condition for maze generation

import Html exposing (..)
import List.Extra as ListE
import Random exposing (Generator)
import Random.List


type alias Model =
    { board : Board
    , pointToDig : Coordinate
    , directionToDig : Direction
    }


type alias Board =
    List Point


type alias Point =
    { coordinate : Coordinate
    , pointStatus : PointStatus
    }


type alias Coordinate =
    { x : Int
    , y : Int
    }


type PointStatus
    = Wall
    | Road


init : ( Model, Cmd Msg )
init =
    let
        newBoard =
            initializeBoard 8

        initialCoordinate =
            newBoard
                |> List.head
                |> Maybe.andThen (\p -> Just (Coordinate p.coordinate.x p.coordinate.y))
                |> Maybe.withDefault (Coordinate 0 0)
    in
    ( Model newBoard initialCoordinate UP
    , Random.generate ChoosePoint (choosePoint newBoard)
    )



-- initialize board


initializeBoard : Int -> Board
initializeBoard numOfPoint =
    numOfPoint
        |> List.range 0
        |> initializePoints numOfPoint


initializePoints : Int -> List Int -> Board
initializePoints numOfPoint width =
    ListE.lift2 (initializePoint numOfPoint) width width


initializePoint : Int -> Int -> Int -> Point
initializePoint numOfPoint x y =
    Point (Coordinate x y)
        (if isPerimeter numOfPoint x y then
            Road

         else
            Wall
        )


isPerimeter : Int -> Int -> Int -> Bool
isPerimeter numOfPoint x y =
    x == 0 || y == 0 || x == numOfPoint || y == numOfPoint


choosePoint : Board -> Generator ( Maybe Point, List Point )
choosePoint board =
    Random.List.choose (List.filter isOdd board)


isOdd : Point -> Bool
isOdd point =
    let
        modBy2 =
            modBy 2
    in
    modBy2 point.coordinate.x == 1 && modBy2 point.coordinate.y == 1


type Direction
    = UP
    | DOWN
    | RIGHT
    | LEFT


chooseDirection : Generator Direction
chooseDirection =
    Random.uniform UP [ DOWN, RIGHT, LEFT ]



-- UPDATE


type Msg
    = ChoosePoint ( Maybe Point, List Point )
    | ChooseDirection Direction
    | GeneratingMaze
    | FinishedGenerateMaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChoosePoint ( Just point, _ ) ->
            case diggingThePointToDig model.board point.coordinate of
                Just board ->
                    ( { model | board = board, pointToDig = Coordinate point.coordinate.x point.coordinate.y }
                    , Random.generate ChooseDirection chooseDirection
                    )

                Nothing ->
                    ( model, Random.generate ChoosePoint (choosePoint model.board) )

        ChoosePoint ( Nothing, _ ) ->
            update FinishedGenerateMaze model

        ChooseDirection direction ->
            update GeneratingMaze { model | directionToDig = direction }

        GeneratingMaze ->
            generateMaze model

        FinishedGenerateMaze ->
            ( model, Cmd.none )


diggingThePointToDig : Board -> Coordinate -> Maybe Board
diggingThePointToDig board coordinate =
    ListE.findIndex ((\c p -> c == p.coordinate) coordinate) board
        |> Maybe.andThen (\idx -> Just (ListE.updateAt idx (\p -> { p | pointStatus = Road }) board))


diggingSpecifiedDirection : Board -> Coordinate -> Coordinate -> Maybe Board
diggingSpecifiedDirection board c2 c1 =
    if List.any (\p -> c2 == p.coordinate && p.pointStatus == Wall) board then
        Just (ListE.updateIf (\p -> c2 == p.coordinate || c1 == p.coordinate && p.pointStatus == Wall) (\p -> { p | pointStatus = Road }) board)

    else
        Nothing


generateMaze : Model -> ( Model, Cmd Msg )
generateMaze model =
    let
        ( c2, c1 ) =
            case model.directionToDig of
                UP ->
                    ( Coordinate model.pointToDig.x (model.pointToDig.y + 2)
                    , Coordinate model.pointToDig.x (model.pointToDig.y + 1)
                    )

                DOWN ->
                    ( Coordinate model.pointToDig.x (model.pointToDig.y - 2)
                    , Coordinate model.pointToDig.x (model.pointToDig.y - 1)
                    )

                RIGHT ->
                    ( Coordinate (model.pointToDig.x + 2) model.pointToDig.y
                    , Coordinate (model.pointToDig.x + 1) model.pointToDig.y
                    )

                LEFT ->
                    ( Coordinate (model.pointToDig.x - 2) model.pointToDig.y
                    , Coordinate (model.pointToDig.x - 1) model.pointToDig.y
                    )
    in
    case diggingSpecifiedDirection model.board c2 c1 of
        Just board ->
            ( { model | board = board, pointToDig = c2 }, Random.generate ChooseDirection chooseDirection )

        Nothing ->
            ( model, Random.generate ChoosePoint (choosePoint model.board) )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    text "game"
