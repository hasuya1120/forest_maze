module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as ListE
import Random exposing (Generator)
import Random.List
import Url exposing (toString)


type alias Model =
    { board : Board
    , directions : List Direction
    , candidates : Candidates
    , maxOfCoordinate : Int
    , startPoint : Coordinate
    , goalPoint : Coordinate
    }


type alias Board =
    List Point


type alias Candidates =
    List Point


type alias Point =
    { coordinate : Coordinate
    , pointStatus : PointStatus
    , possibleDirections : List Direction
    }


type alias Coordinate =
    { x : Int
    , y : Int
    }


type PointStatus
    = Wall
    | Road


type Direction
    = UP
    | DOWN
    | RIGHT
    | LEFT


allDirections : List Direction
allDirections =
    [ UP, DOWN, RIGHT, LEFT ]


init : ( Model, Cmd Msg )
init =
    let
        newBoard =
            initializeBoard 30

        candidates =
            initializeCandidate newBoard
    in
    ( Model newBoard [] candidates 30 (Coordinate 0 0) (Coordinate 0 0)
    , Random.generate ChooseCandidate (chooseCandidate candidates)
    )



-- initialize board


initializeBoard : Int -> Board
initializeBoard maxOfCoordinate =
    maxOfCoordinate
        |> List.range 0
        |> initializePoints maxOfCoordinate


initializePoints : Int -> List Int -> Board
initializePoints maxOfCoordinate width =
    ListE.lift2 (initializePoint maxOfCoordinate) width width


initializePoint : Int -> Int -> Int -> Point
initializePoint maxOfCoordinate y x =
    Point (Coordinate x y)
        (if isPerimeter maxOfCoordinate x y then
            Road

         else
            Wall
        )
        allDirections


isPerimeter : Int -> Int -> Int -> Bool
isPerimeter maxOfCoordinate x y =
    x == 0 || y == 0 || x == maxOfCoordinate || y == maxOfCoordinate


isOdd : Point -> Bool
isOdd point =
    let
        modBy2 =
            modBy 2
    in
    modBy2 point.coordinate.x == 1 && modBy2 point.coordinate.y == 1


initializeCandidate : Board -> Candidates
initializeCandidate board =
    List.filter (\p -> isOdd p) board


chooseCandidate : Candidates -> Generator ( Maybe Point, List Point )
chooseCandidate candidates =
    Random.List.choose candidates


chooseGoal : Candidates -> Generator ( Maybe Point, List Point )
chooseGoal candidates =
    chooseCandidate candidates


chooseDirection : Generator Direction
chooseDirection =
    Random.uniform UP [ DOWN, RIGHT, LEFT ]


chooseDirections : Int -> Generator (List Direction)
chooseDirections maxOfCoordinate =
    Random.list ((maxOfCoordinate ^ 2) * 2) chooseDirection



-- UPDATE


type Msg
    = ChooseCandidate ( Maybe Point, List Point )
    | ChooseDirections (List Direction)
    | GeneratingMaze
    | ChooseGoal ( Maybe Point, List Point )
    | FinishedGeneratingMaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseCandidate ( Just candidate, _ ) ->
            ( { model | candidates = [ candidate ], startPoint = candidate.coordinate }
            , Random.generate ChooseDirections (chooseDirections model.maxOfCoordinate)
            )

        ChooseCandidate ( Nothing, _ ) ->
            ( { model | candidates = [] }
            , Random.generate ChooseDirections (chooseDirections model.maxOfCoordinate)
            )

        ChooseDirections directions ->
            update GeneratingMaze { model | directions = directions }

        GeneratingMaze ->
            update FinishedGeneratingMaze (generateMaze model)

        FinishedGeneratingMaze ->
            let
                newBoard =
                    makeWall model.maxOfCoordinate model.board
            in
            ( { model | board = newBoard }
            , Random.generate ChooseGoal (chooseGoal (listGoalCandidates model.startPoint newBoard))
            )

        ChooseGoal ( Just candidate, _ ) ->
            ( { model | goalPoint = candidate.coordinate }, Cmd.none )

        ChooseGoal ( Nothing, _ ) ->
            ( model, Cmd.none )


listGoalCandidates : Coordinate -> Board -> Candidates
listGoalCandidates startPoint board =
    List.filter (\p -> isCoordinateMoreThan5PointAway startPoint p.coordinate && p.pointStatus == Road) board


isCoordinateMoreThan5PointAway : Coordinate -> Coordinate -> Bool
isCoordinateMoreThan5PointAway startPoint point =
    abs (point.x - startPoint.x) > 5 && abs (point.y - startPoint.y) > 5


makeWall : Int -> Board -> Board
makeWall numOfPint board =
    List.map
        (\p ->
            if isPerimeter numOfPint p.coordinate.x p.coordinate.y then
                { p | pointStatus = Wall }

            else
                p
        )
        board


generateMaze : Model -> Model
generateMaze model =
    let
        direction =
            Maybe.withDefault UP (List.head model.directions)

        newDirections =
            case model.directions of
                d :: ds ->
                    ds ++ [ d ]

                _ ->
                    model.directions
    in
    case model.candidates of
        [] ->
            model

        point :: points ->
            let
                ( c2, c1 ) =
                    case direction of
                        UP ->
                            ( Coordinate point.coordinate.x (point.coordinate.y + 2)
                            , Coordinate point.coordinate.x (point.coordinate.y + 1)
                            )

                        DOWN ->
                            ( Coordinate point.coordinate.x (point.coordinate.y - 2)
                            , Coordinate point.coordinate.x (point.coordinate.y - 1)
                            )

                        RIGHT ->
                            ( Coordinate (point.coordinate.x + 2) point.coordinate.y
                            , Coordinate (point.coordinate.x + 1) point.coordinate.y
                            )

                        LEFT ->
                            ( Coordinate (point.coordinate.x - 2) point.coordinate.y
                            , Coordinate (point.coordinate.x - 1) point.coordinate.y
                            )
            in
            case diggingSpecifiedDirection point.coordinate c1 c2 model.board of
                Just board ->
                    let
                        nextCandidates =
                            Maybe.withDefault point (ListE.find (\p -> p.coordinate == c2) model.board)
                                |> (\p -> p :: model.candidates)
                    in
                    generateMaze { model | board = board, candidates = nextCandidates, directions = newDirections }

                Nothing ->
                    let
                        newPoint =
                            { point | possibleDirections = ListE.remove direction point.possibleDirections }
                    in
                    if List.isEmpty newPoint.possibleDirections then
                        generateMaze { model | candidates = points, directions = newDirections }

                    else
                        generateMaze { model | candidates = newPoint :: points, directions = newDirections }


diggingSpecifiedDirection : Coordinate -> Coordinate -> Coordinate -> Board -> Maybe Board
diggingSpecifiedDirection c0 c1 c2 board =
    if List.any (\p -> c2 == p.coordinate && p.pointStatus == Wall) board then
        Just (ListE.updateIf (\p -> c0 == p.coordinate || c2 == p.coordinate || c1 == p.coordinate && p.pointStatus == Wall) (\p -> { p | pointStatus = Road }) board)

    else
        Nothing



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        (List.map
            (\p ->
                if p.coordinate == model.startPoint then
                    div [ class "start_point" ] [ text "🏝️" ]

                else if p.coordinate == model.goalPoint then
                    div [ class "goal_point" ] [ text "🏕️" ]

                else
                    case p.pointStatus of
                        Wall ->
                            div [ class "wall" ] [ text "🌲" ]

                        Road ->
                            div [ class "road" ] [ text "☘️" ]
            )
            model.board
        )
