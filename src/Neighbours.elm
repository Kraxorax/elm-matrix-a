module Matrix.Neighbours exposing (MatrixTopology(..), neighbours)

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

import List as L exposing (..)
import Matrix exposing (..)


{-| Possible topologies
-}
type MatrixTopology
    = Plane
    | Torus
    | StripHorizontal
    | StripVertical


{-| Given topology and (x, y) returns list of
neighbours from given
-}
neighbours : MatrixTopology -> Int -> Int -> Matrix a -> List a
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


neighboursOnTorus : Int -> Int -> Matrix a -> List a
neighboursOnTorus x y m =
    let
        mw =
            width m

        mh =
            height m

        r1 =
            Maybe.withDefault [] (getRow (modulo (y - 1) mh) m)

        r2 =
            Maybe.withDefault [] (getRow y m)

        r3 =
            Maybe.withDefault [] (getRow (modulo (y + 1) mh) m)

        start =
            modulo (x - 1) mw

        end =
            modulo (x + 1) mw
    in
    unboundHorizontalSide start end r1
        |> L.append (unboundHorizontalCenter start end r2)
        |> L.append (unboundHorizontalSide start end r3)


neighboursOnPlane : Int -> Int -> Matrix a -> List a
neighboursOnPlane x y m =
    let
        startRow =
            max (y - 1) 0

        numToTakeRows =
            if (y - 1) < 0 then
                3 + (y - 1)

            else
                3

        rows =
            L.drop startRow m |> L.take numToTakeRows

        start =
            x - 1

        end =
            x + 1

        numToTakeCells =
            if start < 0 then
                3 + start

            else
                3
    in
    rows
        |> L.indexedMap
            (\yc row ->
                if startRow + yc == y then
                    boundHorizontalCenter start end row

                else
                    boundHorizontalSide start numToTakeCells row
            )
        |> L.concat


neighboursOnVerticalStrip : Int -> Int -> Matrix a -> List a
neighboursOnVerticalStrip x y m =
    let
        mw =
            width m

        mh =
            height m

        rowSides =
            Maybe.withDefault [] (getRow (modulo (y - 1) mh) m)
                :: Maybe.withDefault
                    []
                    (getRow (modulo (y + 1) mh) m)
                :: []

        rowCenter =
            Maybe.withDefault [] (getRow y m)

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
                |> L.map
                    (\row ->
                        boundHorizontalSide start numToTakeCells row
                    )
                |> concat

        centerNbrs =
            boundHorizontalCenter start end rowCenter
    in
    sideNbrs
        |> L.append centerNbrs


neighboursOnHorizontalStrip : Int -> Int -> Matrix a -> List a
neighboursOnHorizontalStrip x y m =
    let
        mw =
            width m

        mh =
            height m

        rowCenter =
            getRow y m |> Maybe.withDefault []

        rowSides =
            (getRow (y - 1) m :: [ getRow (y + 1) m ])
                |> L.map (Maybe.withDefault [])

        start =
            modulo (x - 1) mw

        end =
            modulo (x + 1) mw
    in
    unboundHorizontalCenter start end rowCenter
        |> L.append
            (rowSides
                |> L.map (unboundHorizontalSide start end)
                |> L.concat
            )


modulo : Int -> Int -> Int
modulo a m =
    if a < 0 then
        m + a

    else if a >= m then
        m - a

    else
        a


unboundHorizontalSide : Int -> Int -> List a -> List a
unboundHorizontalSide start end l =
    if end < start then
        L.take (end + 1) l
            |> L.append
                (L.drop start l
                    |> L.take 3
                )

    else
        L.drop start l |> L.take 3


unboundHorizontalCenter : Int -> Int -> List a -> List a
unboundHorizontalCenter start end l =
    L.drop start l
        |> L.take 1
        |> L.append (L.drop end l |> L.take 1)


boundHorizontalCenter : Int -> Int -> List a -> List a
boundHorizontalCenter start end l =
    if start < 0 then
        L.drop end l |> L.take 1

    else
        L.drop start l
            |> L.take 1
            |> L.append (L.drop end l |> L.take 1)


boundHorizontalSide : Int -> Int -> List a -> List a
boundHorizontalSide start toTake l =
    L.drop start l |> L.take toTake
