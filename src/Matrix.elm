module Matrix exposing
    ( Matrix
    , repeat
    , generate
    , map
    , indexedMap
    , foldr
    , foldl
    , concatHorizontal
    , concatVertical
    , width
    , height
    , get
    , getRow
    , getColumn
    , set
    , toArray
    )

{-| Matrix (a) datastructure of certain width and height,
containing elements of type (a) on x and y indexes.

Exposes Matrix creation, traversal, and some manipulation functions.


# Definition

Matrix is just nested array with a constraint that all sub-arrays must be of same length

@docs Matrix


# Creation

Use `repeat` and `generate` to create your matrices.

@docs repeat

@docs generate


# Traversal

These are just standard traversal functions.

@docs map

@docs indexedMap

@docs foldr

@docs foldl


# Manipulation

@docs concatHorizontal

@docs concatVertical


# Utility

@docs width

@docs height

@docs get

@docs getRow

@docs getColumn

@docs toArray

-}

import Array as A exposing (..)


{-| This is not a way to create a matrix, just a type alias.
For creation use some of cretion functions.
-}
type alias Matrix a =
    Array (Array a)


{-| Creates Matrix of given width and height
by repeating single given value.

Call `repeat` with width and height, and a value to be repeated.


    myFourByThreeMatrixOfZeros =
        repeat 4 3 0

    -- 0 0 0 0
    -- 0 0 0 0
    -- 0 0 0 0

-}
repeat : Int -> Int -> a -> Matrix a
repeat w h =
    A.repeat w >> A.repeat h


{-| Creates Matrix of given width and height by calling a generator function.

Call `generate` with width and height, and a _generator function_ `: Int -> Int -> a`.
Generator function will be called for every element of matrix and will receive `x` and `y` of that element as params to return a value to be put into matrix.


    generate : Int -> Int -> (Int -> Int -> a) -> Matrix a

    multiplicationTable =
        generate 10 10 (\x y -> (x + 1) * (y + 1))

    --  1   2   3   4   5   6   7   8   9   10
    --  2   4   6   8   10  12  14  16  18  20
    --  3   6   9   12  15  18  21  24  27  30
    --  4   8   12  16  20  24  28  32  36  40
    --  5   10  15  20  25  30  35  40  45  50
    --  6   12  18  24  30  36  42  48  54  60
    --  7   14  21  28  35  42  49  56  63  70
    --  8   16  24  32  40  48  56  64  72  80
    --  9   18  27  36  45  54  63  72  81  90
    --  10  20  30  40  50  60  70  80  90  100

-}
generate : Int -> Int -> (Int -> Int -> a) -> Matrix a
generate w h f =
    A.map
        (\y ->
            A.map
                (\x -> f x y)
                (A.initialize w identity)
        )
        (A.initialize h identity)


{-| indexedMap will call your function with `x`, `y` and `a` as params

    myMatrix = repeat 3 3 2
    -- 2 2 2
    -- 2 2 2
    -- 2 2 2
    indexedMap (\x y element ->
        (x + y) * element |> String.fromInt)
        myMatrix
    --  "0" "2" "4"
    --  "2" "4" "6"
    --  "4" "6" "8"

-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f =
    A.indexedMap
        (\y row ->
            A.indexedMap
                (\x ele -> f x y ele)
                row
        )


{-| Jup, it's a map

    myMatrix = repeat 3 3 0
    -- 0 0 0
    -- 0 0 0
    -- 0 0 0
    map (String.fromInt) myMatrix
    --  "0" "0" "0"
    --  "0" "0" "0"
    --  "0" "0" "0"

-}
map : (a -> b) -> Matrix a -> Matrix b
map f =
    A.map
        (\row ->
            A.map
                (\ele -> f ele)
                row
        )


{-| Folding right

    myMatrix = generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    foldr (++) "" myMatrix
    --  "00102030011121310212223203132333"

-}
foldr : (a -> b -> b) -> b -> Matrix a -> b
foldr f =
    A.foldr
        (\row bc ->
            A.foldr
                (\ele bcc -> f ele bcc)
                bc
                row
        )


{-| Folding left

    myMatrix = generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    foldl (++) "" myMatrix
    --  "33231303322212023121110130201000"

-}
foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl f =
    A.foldl
        (\row bc ->
            A.foldl
                (\ele bcc -> f ele bcc)
                bc
                row
        )


{-| Returns height of given Matrix

    myMatrix = repeat 2 3 0
    -- 0 0
    -- 0 0
    -- 0 0
    height myMatrix
    -- 3

-}
height : Matrix a -> Int
height =
    A.length


{-| Returns width of given Matrix

    myMatrix = repeat 2 3 0
    -- 0 0
    -- 0 0
    -- 0 0
    width myMatrix
    -- 2

-}
width : Matrix a -> Int
width =
    A.get 0 >> Maybe.withDefault A.empty >> A.length


{-| Concatinates two matrices horizontally.
Will return Result Err if matrices are not of same height.

    matrixOne = repeat 2 2 1
    -- 1 1
    -- 1 1
    matrixTwo = repeat 2 2 2
    -- 2 2
    -- 2 2
    concatHorizontal matrixOne matrixTwo
    -- 1 1 2 2
    -- 1 1 2 2

-}
concatHorizontal : Matrix a -> Matrix a -> Result String (Matrix a)
concatHorizontal m n =
    if height m == height n then
        Ok <|
            A.indexedMap
                (\i mrow ->
                    getRow i n
                        |> Result.withDefault A.empty
                        |> A.append mrow
                )
                m

    else
        Err "Matrix: matrices are of different height"


{-| Concatinates two matrices vertically.
Will return Result Err if matrices are not of same width.

    matrixOne = repeat 2 2 1
    -- 1 1
    -- 1 1
    matrixTwo = repeat 2 2 2
    -- 2 2
    -- 2 2
    concatVertical matrixOne matrixTwo
    -- 1 1
    -- 1 1
    -- 2 2
    -- 2 2

-}
concatVertical : Matrix a -> Matrix a -> Result String (Matrix a)
concatVertical m n =
    if width m == width n then
        Ok <| A.append m n

    else
        Err "Matrix: matrices are of different width"


{-| Returns element at given x and y from matrix.
Nothing of indexes are out of bounds.

    myMatrix = generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    get 1 2 myMatrix
    -- "12"

-}
get : Int -> Int -> Matrix a -> Result String a
get x y m =
    case m |> (A.get y >> Maybe.withDefault A.empty >> A.get x) of
        Nothing ->
            Err "Matrix: Location out of bounds"

        Just a ->
            Ok a


{-| Returns a row at given index as a list.
Result Err if index is out of bounds.

    myMatrix = generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    getRow 1 myMatrix
    --  Array.fromList ["01","11","21","31"]

-}
getRow : Int -> Matrix a -> Result String (Array a)
getRow y m =
    case A.get y m of
        Just x ->
            Ok x

        Nothing ->
            Err "Matrix: Row index out of bounds"


{-| Returns a column at given index as an array.
Result Err if index is out of bounds.

    myMatrix =
        generate 4 4 (\x y -> String.fromInt x ++ String.fromInt y)

    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    getColumn 1 myMatrix
    --  Array.fromList ["10", "11", "12", "13"]

-}
getColumn : Int -> Matrix a -> Result String (Array a)
getColumn x m =
    A.foldr
        (\row c -> A.push (A.get x row) c)
        A.empty
        m
        |> arrMb2ResArr




{-| Set the element at a particular index. Returns an updated array. 
If the index is out of range, the matrix is unaltered.

    myMatrix =
        generate 4 4 (\x y -> String.fromInt x ++ String.fromInt y)
            |> set 1 2 "**"

    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "**"    "22"    "32"
    --  "03"    "13"    "23"    "33"

-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set x y a m =
    case A.get y m of
        Just row ->
            A.set y (A.set x a row) m

        Nothing ->
            m


{-| Returns all elements of matrix in a single array.

    myMatrix =
        generate 4 4 (\x y -> String.fromInt x ++ String.fromInt y)

    --  "00"    "10"    "20"    "30"
    --  "01"    "11"    "21"    "31"
    --  "02"    "12"    "22"    "32"
    --  "03"    "13"    "23"    "33"
    toArray myMatrix
    --  Array.fromList ["00", "10", "20", "30", "01", "11", "21", "31", "02", "12", "22", "32", "03", "13", "23", "33"]

-}
toArray : Matrix a -> Array a
toArray =
    A.foldr A.append A.empty



{- a helper -}


arrMb2ResArr : Array (Maybe a) -> Result String (Array a)
arrMb2ResArr =
    A.foldr
        (\a r ->
            case r of
                Err msg ->
                    Err msg

                Ok re ->
                    case a of
                        Just x ->
                            Ok (A.push x re)

                        Nothing ->
                            Err "Matrix: Column index out of bounds"
        )
        (Ok A.empty)
