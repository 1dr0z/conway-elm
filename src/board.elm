module Board
    exposing
        ( Status(Alive, Dead)
        , isAlive
        , isDead
        , toggle
        , Point
        , Board
        , get
        , set
        , clear
        , initialize
        , fromList
        , toList
        , stringify
        , nextBoard
        )

import Array exposing (Array)
import Debug


-- Status


type Status
    = Alive
    | Dead


isAlive : Status -> Bool
isAlive status =
    status == Alive


isDead : Status -> Bool
isDead status =
    status == Dead


toggle : Status -> Status
toggle status =
    if status == Alive then
        Dead
    else
        Alive



-- Point


type alias Point =
    ( Int, Int )


rowIndex : Board -> Int -> Int
rowIndex { cols } row =
    row * cols


toIndex : Board -> Point -> Maybe Int
toIndex board ( x, y ) =
    if (x < 0 || x >= board.rows) then
        Nothing
    else if (y < 0 || y >= board.cols) then
        Nothing
    else
        Just ((rowIndex board x) + y)


fromIndex : Board -> Int -> Maybe Point
fromIndex board index =
    if inBounds board index then
        Just ( index // board.cols, index % board.cols )
    else
        Nothing


inBounds : Board -> Int -> Bool
inBounds board index =
    index >= 0 && index < Array.length board.array



-- Board


type alias Board =
    { array : Array Status
    , rows : Int
    , cols : Int
    }


get : Board -> Point -> Maybe Status
get board point =
    toIndex board point
        |> Maybe.andThen (\index -> Array.get index board.array)


set : Board -> Point -> Status -> Board
set board point status =
    let
        index =
            toIndex board point |> Maybe.withDefault -1
    in
        { board | array = Array.set index status board.array }


clear : Board -> Board
clear board =
    { board
        | array = Array.map (\_ -> Dead) board.array
    }


initialize : ( Int, Int ) -> Board
initialize ( rows, cols ) =
    { array = Array.repeat (rows * cols) Dead
    , rows = rows
    , cols = cols
    }


fromList : List (List Status) -> Board
fromList list =
    let
        first =
            List.head list |> Maybe.withDefault []

        cols =
            List.length first

        flat =
            List.concat list

        rows =
            List.length flat // cols
    in
        { array = Array.fromList flat, rows = rows, cols = cols }


getRow : Board -> Int -> List Status
getRow board row =
    let
        head =
            rowIndex board row

        tail =
            head + board.cols
    in
        Array.slice head tail board.array |> Array.toList


toList : Board -> List (List Status)
toList board =
    List.range 0 (board.rows - 1)
        |> List.map (getRow board)


stringify : Board -> String
stringify board =
    toList board
        |> List.map toString
        |> String.join "\n"



-- Game


validPoint : Board -> Point -> Bool
validPoint board point =
    toIndex board point
        |> Maybe.map (inBounds board)
        |> Maybe.withDefault False


neighbors : Board -> Point -> List Point
neighbors board ( x, y ) =
    let
        possibles =
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            ]
    in
        List.filter (validPoint board) possibles


aliveNeighborCount : Board -> Point -> Int
aliveNeighborCount board point =
    neighbors board point
        |> List.map (get board)
        |> List.filterMap identity
        |> List.filter isAlive
        |> List.length


nextStatus : Status -> Int -> Status
nextStatus status alive =
    if status == Dead && alive == 3 then
        Alive
    else if status == Alive && (alive < 2 || alive > 3) then
        Dead
    else if status == Alive && (alive == 2 || alive == 3) then
        Alive
    else
        status


nextStatusAt : Board -> Int -> Status -> Status
nextStatusAt board index status =
    fromIndex board index
        |> Maybe.map (aliveNeighborCount board)
        |> Maybe.map (nextStatus status)
        |> Maybe.withDefault Dead


nextBoard : Board -> Board
nextBoard board =
    { board
        | array = Array.indexedMap (nextStatusAt board) board.array
    }
