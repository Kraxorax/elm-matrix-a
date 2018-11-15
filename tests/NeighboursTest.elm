module NeighboursTest exposing (neighboursTestSuite)

import Array as A exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Matrix as M exposing (..)
import Neighbours as N exposing (..)
import Test exposing (..)


rand1to10M : Fuzzer Int
rand1to10M =
    intRange 1 1000


testMatrix : Matrix String
testMatrix =
    M.generate 4 4 (\x y -> String.fromInt x ++ String.fromInt y)


haveSameElements : Array String -> Array String -> Bool
haveSameElements a b =
    if A.length a /= A.length b then
        False

    else
        A.foldr
            (\ea x -> x && A.foldr (\eb y -> y || eb == ea) False b)
            True
            a



--  Plane


planeNeighbours_00 =
    [ "01", "11", "10" ] |> A.fromList


planeNeighbours_33 =
    [ "22", "32", "23" ] |> A.fromList


planeNeighbours_11 =
    [ "00", "10", "20", "01", "21", "02", "12", "22" ] |> A.fromList



-- Torus


torusNeighbours_00 =
    [ "01", "11", "10", "30", "31", "03", "13", "33" ] |> A.fromList


torusNeighbours_11 =
    [ "00", "10", "20", "01", "21", "02", "12", "22" ] |> A.fromList


torusNeighbours_33 =
    [ "22", "32", "02", "23", "03", "00", "20", "30" ] |> A.fromList



-- Strips


stripHorizontalNeighbours_00 =
    [ "01", "11", "10", "30", "31" ] |> A.fromList


stripVerticalNeighbours_00 =
    [ "01", "11", "10", "03", "13" ] |> A.fromList


arrayToString : Array String -> String
arrayToString =
    A.foldl (\a x -> a ++ "_" ++ x) ""


neighboursTestSuite : Test
neighboursTestSuite =
    describe "Matrix Neighbours"
        [ test
            "Plane 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.Plane 0 0 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n planeNeighbours_00)
            )
        , test
            "Plane 1 1"
            (\_ ->
                let
                    n =
                        N.neighbours N.Plane 1 1 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n planeNeighbours_11)
            )
        , test
            "Plane 3 3"
            (\_ ->
                let
                    n =
                        N.neighbours N.Plane 3 3 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n planeNeighbours_33)
            )
        , test
            "Torus 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.Torus 0 0 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n torusNeighbours_00)
            )
        , test
            "Torus 1 1"
            (\_ ->
                let
                    n =
                        N.neighbours N.Torus 1 1 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n torusNeighbours_11)
            )
        , test
            "Torus 3 3"
            (\_ ->
                let
                    n =
                        N.neighbours N.Torus 3 3 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n torusNeighbours_33)
            )
        , test
            "Horizontal strip 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.StripHorizontal 0 0 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n stripHorizontalNeighbours_00)
            )
        , test
            "Vertical strip 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.StripVertical 0 0 testMatrix
                in
                Expect.true (arrayToString n) (haveSameElements n stripVerticalNeighbours_00)
            )
        ]
