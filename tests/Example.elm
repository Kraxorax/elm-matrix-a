module Example exposing (numberOfDigits, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Matrix as M exposing (..)
import Test exposing (..)


rand1to10M : Fuzzer Int
rand1to10M =
    intRange 1 1000


suite : Test
suite =
    describe "Matrix basics"
        [ fuzz3 rand1to10M
            rand1to10M
            rand1to10M
            "Creates Matrix of proper size"
            (\w h a ->
                let
                    m =
                        M.repeat w h a

                    mw =
                        M.width m

                    mh =
                        M.height m
                in
                Expect.equal ( w, h ) ( mw, mh )
            )
        , fuzz3 rand1to10M
            rand1to10M
            rand1to10M
            "Map and foldr"
            (\w h a ->
                let
                    m =
                        M.repeat w h a

                    mm =
                        M.map (\x -> String.fromInt x) m

                    sm =
                        M.foldr (++) "" mm

                    lm =
                        numberOfDigits a * (w * h)
                in
                Expect.equal lm (String.length sm)
            )
        , fuzz3 rand1to10M
            rand1to10M
            rand1to10M
            "Generate and indexedMap"
            (\w h a ->
                let
                    m =
                        M.generate w h (\x y -> ( x, y ))

                    mm =
                        M.indexedMap (\x y ( ex, ey ) -> x == ex && y == ey) m

                    res =
                        M.foldl (&&) True mm
                in
                Expect.true "Generate and indexedMap" res
            )
        , fuzz2 rand1to10M
            rand1to10M
            "Concationate horizontal is ok"
            (\w h ->
                let
                    m1 =
                        M.repeat w h 0

                    m2 =
                        M.repeat w h 1
                in
                concatHorizontal m1 m2
                    |> Expect.ok
            )
        , fuzz2 rand1to10M
            rand1to10M
            "Concationate horizontal return Matrix of proper width"
            (\w h ->
                let
                    m1 =
                        M.repeat w h 0

                    m2 =
                        M.repeat w h 1

                    mc =
                        case concatHorizontal m1 m2 of
                            Ok x ->
                                x

                            Err msg ->
                                M.empty
                in
                Expect.equal (w * 2) (width mc)
            )
        , fuzz2 rand1to10M
            rand1to10M
            "Concationate vertical is ok"
            (\w h ->
                let
                    m1 =
                        M.repeat w h 0

                    m2 =
                        M.repeat w h 1
                in
                concatVertical m1 m2
                    |> Expect.ok
            )
        , fuzz2 rand1to10M
            rand1to10M
            "Concationate vertical return Matrix of proper height"
            (\w h ->
                let
                    m1 =
                        M.repeat w h 0

                    m2 =
                        M.repeat w h 1

                    mc =
                        case concatVertical m1 m2 of
                            Ok x ->
                                x

                            Err msg ->
                                M.empty
                in
                Expect.equal (h * 2) (height mc)
            )
        ]


numberOfDigits : Int -> Int
numberOfDigits x =
    case x // 10 of
        0 ->
            1

        n ->
            1 + numberOfDigits n
