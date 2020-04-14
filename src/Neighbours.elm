module Neighbours exposing
    ( MatrixTopology(..)
    , neighbours
    )

{-| Module for getting neighbouring fields of some (x, y).
Works with a few different matrix topologies.
Topology specifies how neighbours are found across matrix edges.

@docs MatrixTopology

@docs neighbours

-}

import Array as A


{-| Possible topologies

    If we mark (0, 0) spot with X in 4x4 Matrix, N marks
    neighbours on given topology.

                            Strip           Strip
    Plane:      Torus:      horizontal:     vertical:

    X N - -     X N - N     X N - N         X N - -
    N N - -     N N - N     N N - N         N N - -
    - - - -     - - - -     - - - -         - - - -
    - - - -     N N - N     - - - -         N N - -

-}
type MatrixTopology
    = Plane
    | Torus
    | StripHorizontal
    | StripVertical


{-| Given a topology and (x, y) returns array of
neighbours from given matrix.

    myMatrix = generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"

    neighbours Plane 0 0 myMatix
    -- Array.fromList ["01","11","10"]

    neighbours Torus 0 0 myMatix
    -- Array.fromList ["33","03","13","31","01","11","10","30"]

    neighbours stripHorizontal 0 0 myMatrix
    -- Array.fromList ["31","01","11","10","30"]

    neighbours StripVertical 0 0 myMatix
    -- Array.fromList ["03","13","01","11","10"]

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
        rowSides =
            getUnboundSideRows y m

        rowCenter =
            orEmpty (getRow y m)

        sideNbrs =
            rowSides
                |> A.map
                    (\row ->
                        unboundHorizontalSide x row
                    )
                |> flatten

        centerNbrs =
            unboundHorizontalCenter x rowCenter
    in
    A.append sideNbrs centerNbrs


neighboursOnPlane : Int -> Int -> Matrix a -> Array a
neighboursOnPlane x y m =
    let
        rowCenter =
            getRow y m |> orEmpty

        rowSides =
            getBoundSideRows y m

        sideNbrs =
            rowSides
                |> A.map
                    (\row ->
                        boundHorizontalSide x row
                    )
                |> flatten

        centerNbrs =
            boundHorizontalCenter x rowCenter
    in
    A.append sideNbrs centerNbrs


neighboursOnVerticalStrip : Int -> Int -> Matrix a -> Array a
neighboursOnVerticalStrip x y m =
    let
        rowSides =
            getUnboundSideRows y m

        rowCenter =
            orEmpty (getRow y m)

        sideNbrs =
            rowSides
                |> A.map
                    (\row ->
                        boundHorizontalSide x row
                    )
                |> flatten

        centerNbrs =
            boundHorizontalCenter x rowCenter
    in
    A.append sideNbrs centerNbrs


neighboursOnHorizontalStrip : Int -> Int -> Matrix a -> Array a
neighboursOnHorizontalStrip x y m =
    let
        rowCenter =
            getRow y m |> orEmpty

        rowSides =
            getBoundSideRows y m

        sideNbrs =
            rowSides
                |> A.map (unboundHorizontalSide x)
                |> flatten

        centerNbrs =
            unboundHorizontalCenter x rowCenter
    in
    A.append sideNbrs centerNbrs


modulo : Int -> Int -> Int
modulo a m =
    if a < 0 then
        m + a

    else if a >= m then
        m - a

    else
        a


unboundHorizontalSide : Int -> Array a -> Array a
unboundHorizontalSide x row =
    let
        mw =
            A.length row

        start =
            modulo (x - 1) mw

        end =
            modulo (x + 1) mw
    in
    if end < start then
        A.slice 0 (end + 1) row
            |> A.append
                (A.slice start (start + 3) row)

    else
        A.slice start end row


unboundHorizontalCenter : Int -> Array a -> Array a
unboundHorizontalCenter x row =
    let
        mw =
            A.length row

        start =
            modulo (x - 1) mw

        end =
            modulo (x + 1) mw
    in
    A.slice start (start + 1) row
        |> A.append (A.slice end (end + 1) row)


boundHorizontalCenter : Int -> Array a -> Array a
boundHorizontalCenter x row =
    let
        start =
            x - 1

        end =
            x + 1
    in
    if start < 0 then
        A.slice end (end + 1) row

    else
        A.slice start (start + 1) row
            |> A.append (A.slice end (end + 1) row)


boundHorizontalSide : Int -> Array a -> Array a
boundHorizontalSide x row =
    let
        start =
            x - 1

        toTake =
            if start < 0 then
                3 + start

            else
                3
    in
    A.slice (max 0 start) (start + toTake + 1) row


getBoundSideRows : Int -> Matrix a -> Array (Array a)
getBoundSideRows y m =
    if y == 0 then
        A.repeat 1 (getRow 1 m |> orEmpty)

    else if y == height m - 1 then
        A.repeat 1 (getRow (y - 1) m |> orEmpty)

    else
        getRow (y - 1) m :: [ getRow (y + 1) m ]
            |> A.fromList
            |> A.map orEmpty


getUnboundSideRows : Int -> Matrix a -> Array (Array a)
getUnboundSideRows y m =
    let
        mh =
            height m
    in
    orEmpty (getRow (modulo (y - 1) mh) m)
        :: Result.withDefault
            A.empty
            (getRow (modulo (y + 1) mh) m)
        :: []
        |> A.fromList


flatten : Array (Array a) -> Array a
flatten =
    A.foldr A.append A.empty

orEmpty: Result x a -> a
orEmpty =
    Result.withDefault A.empty
