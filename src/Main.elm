module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import List.Extra as ListE
import Random exposing (Generator)
import Random.List
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , directions : List Direction
    , candidates : Candidates
    , characterPoint : CharacterPoint
    , startTime : Int
    , elapsedTime : Int
    }


type alias Board =
    { points : BoardPoints
    , startCoordinate : Coordinate
    , goalCoordinate : Coordinate
    , maxOfCoordinate : Int
    }


type alias BoardPoints =
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
    | STAY


type alias CharacterPoint =
    { coordinate : Coordinate
    , isGoalPoint : Bool
    }


allDirections : List Direction
allDirections =
    [ UP, DOWN, RIGHT, LEFT ]


init : ( Model, Cmd Msg )
init =
    let
        defaultCoordinate =
            Coordinate 1 1

        newBoard =
            initializeBoard 30

        candidates =
            initializeCandidate newBoard.points

        defaultTime =
            Time.posixToMillis (Time.millisToPosix 0)
    in
    ( Model newBoard [] candidates (CharacterPoint defaultCoordinate False) defaultTime defaultTime
    , Cmd.batch [ Random.generate ChooseCandidate (chooseCandidate candidates), Task.perform StartTime Time.now ]
    )



-- initialize board


initializeBoard : Int -> Board
initializeBoard maxOfCoordinate =
    { points =
        maxOfCoordinate
            |> List.range 0
            |> initializePoints maxOfCoordinate
    , startCoordinate = Coordinate 1 1
    , goalCoordinate = Coordinate 1 1
    , maxOfCoordinate = maxOfCoordinate
    }


initializePoints : Int -> List Int -> BoardPoints
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


initializeCandidate : BoardPoints -> Candidates
initializeCandidate boardPoints =
    List.filter (\p -> isOdd p) boardPoints


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
    | KeyPressed Direction
    | StartTime Time.Posix
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentBoard =
            model.board
    in
    case msg of
        StartTime startTime ->
            ( { model | startTime = Time.posixToMillis startTime }, Cmd.none )

        Tick now ->
            ( { model | elapsedTime = Time.posixToMillis now - model.startTime }, Cmd.none )

        ChooseCandidate ( Just candidate, _ ) ->
            let
                newBoard =
                    { currentBoard | startCoordinate = candidate.coordinate }
            in
            ( { model
                | candidates = [ candidate ]
                , board = newBoard
                , characterPoint = CharacterPoint candidate.coordinate False
              }
            , Random.generate ChooseDirections (chooseDirections model.board.maxOfCoordinate)
            )

        ChooseCandidate ( Nothing, _ ) ->
            ( { model | candidates = [] }
            , Random.generate ChooseDirections (chooseDirections model.board.maxOfCoordinate)
            )

        ChooseDirections directions ->
            update GeneratingMaze { model | directions = directions }

        GeneratingMaze ->
            update FinishedGeneratingMaze (generateMaze model)

        FinishedGeneratingMaze ->
            let
                newBoard =
                    { currentBoard | points = makeWall currentBoard.maxOfCoordinate currentBoard.points }
            in
            ( { model | board = newBoard }
            , Random.generate ChooseGoal (chooseGoal (listGoalCandidates model.board.startCoordinate newBoard.points))
            )

        ChooseGoal ( Just candidate, _ ) ->
            let
                newBoard =
                    { currentBoard | goalCoordinate = candidate.coordinate }
            in
            ( { model | board = newBoard }, Cmd.none )

        KeyPressed direction ->
            ( { model | characterPoint = moveCharacter model.characterPoint direction model.board }, Cmd.none )

        _ ->
            ( model, Cmd.none )


listGoalCandidates : Coordinate -> BoardPoints -> Candidates
listGoalCandidates startCoordinate boardPoint =
    List.filter (\p -> isCoordinateMoreThan10PointsAway startCoordinate p.coordinate && p.pointStatus == Road) boardPoint


isCoordinateMoreThan10PointsAway : Coordinate -> Coordinate -> Bool
isCoordinateMoreThan10PointsAway coordinate1 coordinate2 =
    abs (coordinate2.x - coordinate1.x) > 10 && abs (coordinate2.y - coordinate1.y) > 10


makeWall : Int -> BoardPoints -> BoardPoints
makeWall numOfPint boardPoints =
    List.map
        (\p ->
            if isPerimeter numOfPint p.coordinate.x p.coordinate.y then
                { p | pointStatus = Wall }

            else
                p
        )
        boardPoints


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

                        _ ->
                            ( point.coordinate, point.coordinate )
            in
            case diggingSpecifiedDirection point.coordinate c1 c2 model.board of
                Just boardPoints ->
                    let
                        nextCandidates =
                            Maybe.withDefault point (ListE.find (\p -> p.coordinate == c2) model.board.points)
                                |> (\p -> p :: model.candidates)

                        currentBoard =
                            model.board

                        newBoard =
                            { currentBoard | points = boardPoints }
                    in
                    generateMaze { model | board = newBoard, candidates = nextCandidates, directions = newDirections }

                Nothing ->
                    let
                        newPoint =
                            { point | possibleDirections = ListE.remove direction point.possibleDirections }
                    in
                    if List.isEmpty newPoint.possibleDirections then
                        generateMaze { model | candidates = points, directions = newDirections }

                    else
                        generateMaze { model | candidates = newPoint :: points, directions = newDirections }


diggingSpecifiedDirection : Coordinate -> Coordinate -> Coordinate -> Board -> Maybe BoardPoints
diggingSpecifiedDirection c0 c1 c2 board =
    if List.any (\p -> c2 == p.coordinate && p.pointStatus == Wall) board.points then
        Just (ListE.updateIf (\p -> c0 == p.coordinate || c2 == p.coordinate || c1 == p.coordinate && p.pointStatus == Wall) (\p -> { p | pointStatus = Road }) board.points)

    else
        Nothing


moveCharacter : CharacterPoint -> Direction -> Board -> CharacterPoint
moveCharacter characterPoint direction board =
    let
        nextPointCoordinate =
            case direction of
                UP ->
                    Coordinate characterPoint.coordinate.x (characterPoint.coordinate.y - 1)

                DOWN ->
                    Coordinate characterPoint.coordinate.x (characterPoint.coordinate.y + 1)

                RIGHT ->
                    Coordinate (characterPoint.coordinate.x + 1) characterPoint.coordinate.y

                LEFT ->
                    Coordinate (characterPoint.coordinate.x - 1) characterPoint.coordinate.y

                _ ->
                    characterPoint.coordinate
    in
    case ListE.find (\p -> p.coordinate == nextPointCoordinate && p.pointStatus == Road) board.points of
        Just _ ->
            if nextPointCoordinate == board.goalCoordinate then
                { characterPoint | coordinate = nextPointCoordinate, isGoalPoint = True }

            else
                { characterPoint | coordinate = nextPointCoordinate }

        Nothing ->
            characterPoint



-- SUBSCRIPTION


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection key =
    case key of
        "a" ->
            KeyPressed LEFT

        "d" ->
            KeyPressed RIGHT

        "w" ->
            KeyPressed UP

        "s" ->
            KeyPressed DOWN

        _ ->
            KeyPressed STAY


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (if model.characterPoint.isGoalPoint then
            [ Sub.none ]

         else
            [ Time.every 100 Tick
            , Browser.Events.onKeyPress keyDecoder
            ]
        )



-- VIEW


isCoordinateWithin1Point : Coordinate -> Coordinate -> Bool
isCoordinateWithin1Point coordinate1 coordinate2 =
    abs (coordinate2.x - coordinate1.x) <= 1 && abs (coordinate2.y - coordinate1.y) <= 1


view : Model -> Html Msg
view model =
    div [ class "game_board" ]
        [ div [ class "container" ]
            (List.map
                (\p ->
                    if model.characterPoint.isGoalPoint || isCoordinateWithin1Point p.coordinate model.characterPoint.coordinate then
                        viewPointWithin1PointWithCharacter p model

                    else
                        div [ class "hidden_point" ] [ text "" ]
                )
                model.board.points
            )
        , if model.characterPoint.isGoalPoint then
            div [ class "menu" ]
                [ text "goal!"
                , br [] []
                , text (String.concat [ "goal time: ", String.fromInt model.elapsedTime, "ms" ])
                ]

          else
            div [ class "menu" ] [ text (String.concat [ "elapsed time: ", String.fromInt model.elapsedTime, "ms" ]) ]
        ]


viewPointWithin1PointWithCharacter : Point -> Model -> Html Msg
viewPointWithin1PointWithCharacter point model =
    if point.coordinate == model.characterPoint.coordinate then
        div [ class "character_point" ] [ text "🐾" ]

    else if point.coordinate == model.board.startCoordinate then
        div [ class "start_point" ] [ text "🏝️" ]

    else if point.coordinate == model.board.goalCoordinate then
        div [ class "goal_point" ] [ text "🏕️" ]

    else
        case point.pointStatus of
            Wall ->
                div [ class "wall" ] [ text "🌲" ]

            Road ->
                div [ class "road" ] [ text "☘️" ]
