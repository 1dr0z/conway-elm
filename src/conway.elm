module Conway exposing (..)

import AnimationFrame
import Board exposing (Board, Status(..), Point)
import Debug
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseUp, onMouseOver)
import Html.Attributes exposing (style, class, classList, value)
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
    | SetSpeed Int


type alias Model =
    { board : Board
    , elapsed : Float
    , speed : Int
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
    , speed = 3
    , tickThreshold = 2000
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
        , speedSetting model.speed
        , iterations model.iterations
        , resetButton
        , nextButton
        , playPauseButton model.paused
        ]


speedSetting : Int -> Html Msg
speedSetting speed =
    select
        [ value <| toString speed
        , onInput speedDecoder
        ]
        [ speedOption 1
        , speedOption 2
        , speedOption 3
        , speedOption 4
        , speedOption 5
        ]


speedDecoder : String -> Msg
speedDecoder value =
    case String.toInt value of
        Ok int ->
            SetSpeed int

        Err _ ->
            NoOp


speedOption : Int -> Html Msg
speedOption speed =
    option [ value <| toString speed ] [ text <| toString speed ]


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
        , style
            [ ( "width", toString model.cellSize ++ "px" )
            , ( "height", toString model.cellSize ++ "px" )
            ]
        , onClick <|
            SetPoint point <|
                Board.toggle status
        , setPointOnDrag point model
        ]
        []


setPointOnDrag : Point -> BoardModel a -> Html.Attribute Msg
setPointOnDrag point model =
    if model.dragging then
        onMouseOver (SetPoint point Alive)
    else
        attribute "_" ""



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
                ( model
                    |> stepGame
                    |> pauseIfUnchanged model.board
                , Cmd.none
                )
            else
                ( timeElapsed delta model.speed model, Cmd.none )

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

        SetSpeed speed ->
            ( setSpeed speed model, Cmd.none )

        PlayPause ->
            ( model
                |> setPause (not model.paused)
                |> stepGame
            , Cmd.none
            )


setSpeed : Int -> Model -> Model
setSpeed speed model =
    { model | speed = speed }


setDragging : Bool -> Model -> Model
setDragging dragging model =
    { model | dragging = dragging }


setPoint : Point -> Status -> Model -> Model
setPoint point status model =
    { model | board = Board.set model.board point status }


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


timeElapsed : Float -> Int -> Model -> Model
timeElapsed delta speed model =
    { model | elapsed = model.elapsed + (delta * toFloat (speed * speed)) }


pauseIfUnchanged : Board -> Model -> Model
pauseIfUnchanged board model =
    { model | paused = (board == model.board) }


setPause : Bool -> Model -> Model
setPause pause model =
    { model | paused = pause }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame



-- Allow controlling the animation speed
-- Allow controlling the size of the grid
