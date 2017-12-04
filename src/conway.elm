module Conway exposing (..)

import AnimationFrame
import Board exposing (Board, Status(Dead, Alive), Point)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseOver)
import Html.Attributes exposing (style, class, classList)
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | Tick Time
    | Frame Time
    | Toggle Point Status
    | ResetBoard
    | PlayPause
    | Dragging Bool


type alias Model =
    { board : Board
    , elapsed : Float
    , tickThreshold : Float
    , cellSize : Int
    , paused : Bool
    , dragging : Bool
    , iterations : Int
    }


defaultModel : Model
defaultModel =
    { board = Board.initialize ( 50, 50 )
    , elapsed = 0
    , tickThreshold = 1000
    , cellSize = 10
    , paused = True
    , dragging = False
    , iterations = 0
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ boardTable model <| Board.toList model.board
        , span [ class "iterations" ] [ text <| toString model.iterations ]
        , button [ onClick ResetBoard ] [ text "Reset" ]
        , button [ onClick (Tick 0) ] [ text "Next" ]
        , button [ onClick PlayPause ]
            [ text <|
                if model.paused then
                    "Run"
                else
                    "Stop"
            ]
        ]


boardTable : Model -> List (List Status) -> Html Msg
boardTable model list =
    table
        [ classList
            [ ( "table", True ) ]
        , onMouseDown (Dragging True)
        , onMouseUp (Dragging False)
        ]
        (List.indexedMap (boardRow model) list)


boardRow : Model -> Int -> List Status -> Html Msg
boardRow model x row =
    tr
        [ classList [ ( "row", True ) ] ]
        (List.indexedMap (\y cell -> boardCell model ( x, y ) cell) row)


boardCell : Model -> Point -> Status -> Html Msg
boardCell model point status =
    td
        [ classList
            [ ( "cell", True )
            , ( "cell-alive", Board.isAlive status )
            , ( "cell-dead", Board.isDead status )
            ]

        -- Make the cell size configurable
        , style
            [ ( "width", toString model.cellSize ++ "px" )
            , ( "height", toString model.cellSize ++ "px" )
            ]

        -- Toggle the cell status on click
        , onClick <|
            Toggle point <|
                Board.toggle status

        -- Make cell alive on hover if in the middle of dragging
        , onMouseOver <|
            if model.dragging then
                Toggle point Alive
            else
                NoOp
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- Triggered on every animation frame
        -- Once enough time has elapsed trigger the next application tick
        Frame dt ->
            if model.paused then
                ( model, Cmd.none )
            else if model.elapsed >= model.tickThreshold then
                ( model, Task.perform Tick Time.now )
            else
                ( { model | elapsed = model.elapsed + dt }
                , Cmd.none
                )

        -- Advance the application by one step
        Tick _ ->
            ( { model
                | board = Board.nextBoard model.board
                , iterations = model.iterations + 1
                , elapsed = 0
              }
            , Cmd.none
            )

        -- Toggle the state of a given cell
        -- The game is paused to allow further modification
        Toggle point status ->
            ( { model
                | board = Board.set model.board point status
                , paused = True
              }
            , Cmd.none
            )

        -- Reset the board and some additional state
        ResetBoard ->
            ( { model
                | board = Board.clear model.board
                , iterations = 0
                , elapsed = 0
                , paused = True
              }
            , Cmd.none
            )

        -- Start / Stop dragging so we can toggle cells with only one click
        Dragging bool ->
            ( { model | dragging = bool }
            , Cmd.none
            )

        -- Toggle Play/Pause state
        PlayPause ->
            ( { model | paused = not model.paused }
            , Task.perform Tick Time.now
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Frame
        ]



-- Allow controlling the animation speed
-- Allow controlling the size of the grid
