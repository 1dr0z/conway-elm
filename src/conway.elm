module Conway exposing (..)

import AnimationFrame
import Board exposing (Board, Status(..), Point)
import Debug
import Html exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseOver)
import Html.Attributes exposing (style, class, classList)
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { init = ( defaultModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | Frame Time
    | StepGame
    | SetPoint Point Status
    | ResetBoard
    | PlayPause
    | SetDragging Bool


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



-- View


view : Model -> Html Msg
view model =
    div []
        [ gameBoard model
        , iterations model.iterations
        , resetButton
        , nextButton
        , playPauseButton model.paused
        ]


iterations : Int -> Html Msg
iterations iterations =
    span [ class "iterations" ] [ text <| toString iterations ]


resetButton : Html Msg
resetButton =
    button [ onClick ResetBoard ] [ text "Reset" ]


nextButton : Html Msg
nextButton =
    button [ onClick StepGame ] [ text "Next" ]


playPauseButton : Bool -> Html Msg
playPauseButton paused =
    button [ onClick PlayPause ]
        [ text <|
            if paused then
                "Run"
            else
                "Stop"
        ]



-- Display Board Game


type alias BoardModel a =
    { a
        | board : Board
        , cellSize : Int
        , dragging : Bool
    }


gameBoard : BoardModel a -> Html Msg
gameBoard model =
    Board.toList model.board
        |> boardTable model


boardTable : BoardModel a -> List (List Status) -> Html Msg
boardTable model list =
    table
        [ class "table"
        , onMouseDown (SetDragging True)
        , onMouseUp (SetDragging False)
        ]
        (List.indexedMap (boardRow model) list)


boardRow : BoardModel a -> Int -> List Status -> Html Msg
boardRow model x row =
    tr
        [ class "row" ]
        (List.indexedMap (\y cell -> boardCell model ( x, y ) cell) row)


boardCell : BoardModel a -> Point -> Status -> Html Msg
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
            SetPoint point <|
                Board.toggle status

        -- Make cell alive on hover if in the middle of dragging
        , onMouseOver <|
            if model.dragging then
                SetPoint point Alive
            else
                NoOp
        ]
        []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Frame delta ->
            if model.paused then
                ( model, Cmd.none )
            else if shouldStep model then
                ( stepGame model, Cmd.none )
            else
                ( timeElapsed delta model, Cmd.none )

        StepGame ->
            ( stepGame model, Cmd.none )

        SetPoint point status ->
            ( model
                |> setPoint point status
                |> setPause True
            , Cmd.none
            )

        ResetBoard ->
            ( model
                |> resetBoard
                |> setPause True
            , Cmd.none
            )

        SetDragging bool ->
            ( setDragging bool model, Cmd.none )

        PlayPause ->
            ( model
                |> togglePause
                |> stepGame
            , Cmd.none
            )


setDragging : Bool -> Model -> Model
setDragging dragging model =
    { model
        | dragging = dragging
    }


setPoint : Point -> Status -> Model -> Model
setPoint point status model =
    { model
        | board = Board.set model.board point status
    }


resetBoard : Model -> Model
resetBoard model =
    { model
        | board = Board.clear model.board
        , iterations = 0
        , elapsed = 0
    }


stepGame : Model -> Model
stepGame model =
    { model
        | board = Board.nextBoard model.board
        , iterations = model.iterations + 1
        , elapsed = 0
    }


shouldStep : Model -> Bool
shouldStep model =
    model.elapsed >= model.tickThreshold


timeElapsed : Float -> Model -> Model
timeElapsed delta model =
    { model
        | elapsed = model.elapsed + delta
    }


togglePause : Model -> Model
togglePause model =
    setPause (not model.paused) model


setPause : Bool -> Model -> Model
setPause pause model =
    { model | paused = pause }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Frame
        ]



-- Allow controlling the animation speed
-- Allow controlling the size of the grid
