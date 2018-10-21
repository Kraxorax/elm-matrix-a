module Neighbours exposing (MatrixTopology(..), neighbours)

{-|

    Module for getting neighbouring fields of some (x, y).
    Works with a few different matrix topologies.
    Topology specifies how neighbours are found across edges.

    If we mark (0, 0) spot with X in 4x4 Matrix, N marks
    neighbours on given topology.

                            Strip           Strip
    Plane:      Torus:      horizontal:     vertical:

    X N - -     X N - N     X N - N         X N - -
    N N - -     N N - N     N N - N         N N - -
    - - - -     - - - -     - - - -         - - - -
    - - - -     N N - N     - - - -         N N - -

-}

import Array as A exposing (..)
import Matrix exposing (..)


{-| Possible topologies
-}
type MatrixTopology
    = Plane
    | Torus
    | StripHorizontal
    | StripVertical


{-| Given topology and (x, y) returns array of
neighbours from given
-}
neighbours : MatrixTopology -> Int -> Int -> Matrix a -> Array a
neighbours mt =
    case mt of
        Plane ->
            neighboursOnPlane

        Torus ->
            neighboursOnTorus

        StripVertical ->
            neighboursOnVerticalStrip

        StripHorizontal ->
            neighboursOnHorizontalStrip


neighboursOnTorus : Int -> Int -> Matrix a -> Array a
neighboursOnTorus x y m =
    let
        mw =
            width m

        mh =
            height m

        r1 =
            Result.withDefault A.empty (getRow (modulo (y - 1) mh) m)

        r2 =
            Result.withDefault A.empty (getRow y m)

        r3 =
            Result.withDefault A.empty (getRow (modulo (y + 1) mh) m)

        start =
            modulo (x - 1) mw

        end =
        
            modulo (x + 1) mw
    in
    unboundHorizontalSide start end r1
        |> A.append (unboundHorizontalCenter start end r2)
        |> A.append (unboundHorizontalSide start end r3)


getBoundSideRows : Int -> Matrix a -> Array (Array a)
getBoundSideRows y m =
    if y == 0 then
        A.repeat 1 (getRow 1 m |> Result.withDefault A.empty)

    else if y == height m - 1 then
        A.repeat 1 (getRow (y - 1) m |> Result.withDefault A.empty)

    else
        (getRow (y - 1) m :: [ getRow (y + 1) m ])
            |> A.fromList
            |> A.map (Result.withDefault A.empty)


neighboursOnPlane : Int -> Int -> Matrix a -> Array a
neighboursOnPlane x y m =
    let
        rowCenter =
            getRow y m |> Result.withDefault A.empty

        rowSides =
            getBoundSideRows y m

        start =
            x - 1

        end =
            x + 1

        numToTakeCells =
            if start < 0 then
                3 + start

            else
                3

        sideNbrs =
            rowSides
                |> A.map
                    (\row ->
                        boundHorizontalSide start numToTakeCells row
                    )
                |> flatten

        centerNbrs =
            boundHorizontalCenter start end rowCenter
    in
    A.append sideNbrs centerNbrs


neighboursOnVerticalStrip : Int -> Int -> Matrix a -> Array a
neighboursOnVerticalStrip x y m =
    let
        mw =
            width m

        mh =
            height m

        rowSides =
            Result.withDefault A.empty (getRow (modulo (y - 1) mh) m)
                :: Result.withDefault
                    A.empty
                    (getRow (modulo (y + 1) mh) m)
                :: []
                |> A.fromList

        rowCenter =
            Result.withDefault A.empty (getRow y m)

        start =
            x - 1

        end =
            x + 1

        numToTakeCells =
            if start < 0 then
                3 + start

            else
                3

        sideNbrs =
            rowSides
                |> A.map
                    (\row ->
                        boundHorizontalSide start numToTakeCells row
                    )
                |> flatten

        centerNbrs =
            boundHorizontalCenter start end rowCenter
    in
    sideNbrs
        |> A.append centerNbrs


neighboursOnHorizontalStrip : Int -> Int -> Matrix a -> Array a
neighboursOnHorizontalStrip x y m =
    let
        mw =
            width m

        mh =
            height m

        rowCenter =
            getRow y m |> Result.withDefault A.empty

        rowSides =
            getBoundSideRows y m

        start =
            modulo (x - 1) mw

        end =
            modulo (x + 1) mw
    in
    unboundHorizontalCenter start end rowCenter
        |> A.append
            (rowSides
                |> A.map (unboundHorizontalSide start end)
                |> flatten
            )


modulo : Int -> Int -> Int
modulo a m =
    if a < 0 then
        m + a

    else if a >= m then
        m - a

    else
        a


unboundHorizontalSide : Int -> Int -> Array a -> Array a
unboundHorizontalSide start end l =
    if end < start then
        A.slice 0 (end + 1) l
            |> A.append
                (A.slice start 3 l)

    else
        A.slice start 3 l


unboundHorizontalCenter : Int -> Int -> Array a -> Array a
unboundHorizontalCenter start end l =
    A.slice start 1 l
        |> A.append (A.slice end 1 l)


boundHorizontalCenter : Int -> Int -> Array a -> Array a
boundHorizontalCenter start end l =
    if start < 0 then
        A.slice end (end + 1) l

    else
        A.slice start (start + 1) l
            |> A.append (A.slice end (end + 1) l)


boundHorizontalSide : Int -> Int -> Array a -> Array a
boundHorizontalSide start toTake l =
    A.slice (max 0 start) (start + toTake + 1) l


flatten : Array (Array a) -> Array a
flatten =
    A.foldr A.append A.empty
