module Board
    exposing
        ( Status(Alive, Dead)
        , isAlive
        , isDead
        , toggle
        , Point
        , Board
        , rows
        , cols
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
rowIndex (Board cols _) row =
    row * cols


toIndex : Board -> Point -> Maybe Int
toIndex board ( x, y ) =
    if (x < 0 || x >= rows board) then
        Nothing
    else if (y < 0 || y >= cols board) then
        Nothing
    else
        Just ((rowIndex board x) + y)


fromIndex : Board -> Int -> Maybe Point
fromIndex ((Board cols _) as board) index =
    if inBounds board index then
        Just ( index // cols, index % cols )
    else
        Nothing


inBounds : Board -> Int -> Bool
inBounds (Board colCount array) index =
    index >= 0 && index < Array.length array



-- Board


type Board
    = Board Int (Array Status)


rows : Board -> Int
rows (Board cols list) =
    (Array.length list) // cols


cols : Board -> Int
cols (Board cols _) =
    cols


get : Board -> Point -> Maybe Status
get ((Board _ array) as board) point =
    toIndex board point
        |> Maybe.andThen (\index -> Array.get index array)


set : Board -> Point -> Status -> Board
set ((Board cols array) as board) point status =
    let
        index =
            toIndex board point |> Maybe.withDefault -1
    in
        Array.set index status array |> Board cols


clear : Board -> Board
clear ((Board cols array) as board) =
    Board cols <| Array.map (\_ -> Dead) array


initialize : ( Int, Int ) -> Board
initialize ( rows, cols ) =
    Array.repeat (rows * cols) Dead |> Board cols


fromList : List (List Status) -> Board
fromList list =
    let
        first =
            List.head list |> Maybe.withDefault []

        cols =
            List.length first

        flat =
            List.concat list
    in
        Array.fromList flat |> Board cols


getRow : Board -> Int -> List Status
getRow ((Board cols array) as board) row =
    let
        head =
            rowIndex board row

        tail =
            head + cols
    in
        Array.slice head tail array |> Array.toList


toList : Board -> List (List Status)
toList ((Board numCols _) as board) =
    List.range 0 ((rows board) - 1)
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
nextBoard ((Board cols array) as board) =
    Board cols <| Array.indexedMap (nextStatusAt board) array
