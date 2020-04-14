module NeighboursTest exposing (neighboursTestSuite)

import Array as A exposing (Array)
import Expect
import Matrix as M exposing (Matrix)
import Neighbours as N

testMatrix : Matrix ( Int, Int )
testMatrix =
    M.generate 4 4 (\x y -> ( x, y ))


haveSameElements : Array ( Int, Int ) -> Array ( Int, Int ) -> Bool
haveSameElements a b =
    if A.length a /= A.length b then
        False

    else
        A.foldr
            (\ea x -> x && A.foldr (\eb y -> y || eb == ea) False b)
            True
            a

planeNeighbours_00 : A.Array a
planeNeighbours_00 =
    [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ) ] |> A.fromList

planeNeighbours_33 : A.Array a
planeNeighbours_33 =
    [ ( 2, 2 ), ( 3, 2 ), ( 2, 3 ) ] |> A.fromList

torusNeighbours_00 : A.Array a
torusNeighbours_00 =
    [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 3, 0 ), ( 3, 1 ), ( 0, 3 ), ( 1, 3 ), ( 3, 3 ) ] |> A.fromList

stripHorizontalNeighbours_00 : A.Array a
stripHorizontalNeighbours_00 =
    [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 3, 0 ), ( 3, 1 ) ] |> A.fromList

stripVerticalNeighbours_00 : A.Array a
stripVerticalNeighbours_00 =
    [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 0, 3 ), ( 1, 3 ) ] |> A.fromList


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
                Expect.true "P-0-0" (haveSameElements n planeNeighbours_00)
            )
        , test
            "Plane 3 3"
            (\_ ->
                let
                    n =
                        N.neighbours N.Plane 3 3 testMatrix
                in
                Expect.true "P-3-3" (haveSameElements n planeNeighbours_33)
            )
        , test
            "Torus 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.Torus 0 0 testMatrix
                in
                Expect.true "0-0" (haveSameElements n torusNeighbours_00)
            )
        , test
            "Horizontal strip 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.StripHorizontal 0 0 testMatrix
                in
                Expect.true "HS-0-0" (haveSameElements n stripHorizontalNeighbours_00)
            )
        , test
            "Vertical strip 0 0"
            (\_ ->
                let
                    n =
                        N.neighbours N.StripVertical 0 0 testMatrix
                in
                Expect.true "HS-0-0" (haveSameElements n stripVerticalNeighbours_00)
            )
        ]
