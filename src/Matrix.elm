module Matrix exposing
    ( Matrix
    , concatHorizontal
    , concatVertical
    , empty
    , foldl
    , foldr
    , generate
    , get
    , getColumn
    , getRow
    , height
    , indexedMap
    , map
    , repeat
    , tm
    , toArray
    , width
    )

{-|

    Matrix (a) datastructure of certain width and height,
    containing elements of type (a) on x and y indexes.

    Exposes Matrix creation, transversal, and some manipulation functions.

-}

import Array as A exposing (..)


{-| Matrix type alias
-}
type alias Matrix a =
    Array (Array a)


{-| Empty Matrix
-}
empty : Matrix a
empty =
    A.empty


{-| Creates Matrix of given width and height
by repeating sigle given value
-}
repeat : Int -> Int -> a -> Matrix a
repeat w h =
    A.repeat w >> A.repeat h


{-| Creates Matrix of given width and height
by calling generator function with current x and y
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


{-| indexedMap will call your function with (x, y, a) params
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
-}
height : Matrix a -> Int
height =
    A.length


{-| Returns width of given Matrix
-}
width : Matrix a -> Int
width =
    A.get 0 >> Maybe.withDefault A.empty >> A.length


{-| Concatinates two Matrixes horizontally.
Will return Nothing if Matrixes are not of same height.
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


{-| Concatinates two Matrixes vertically.
Will return Nothing if Matrixes are not of same width.
-}
concatVertical : Matrix a -> Matrix a -> Result String (Matrix a)
concatVertical m n =
    if width m == width n then
        Ok <| A.append m n

    else
        Err "Matrix: matrices are of different width"


{-| Returns element at given x and y from Matrix.
Nothing of indexes are out of bounds.
-}
get : Int -> Int -> Matrix a -> Result String a
get x y m =
    case m |> (A.get y >> Maybe.withDefault A.empty >> A.get x) of
        Nothing ->
            Err "Matrix: Location out of bounds"

        Just a ->
            Ok a


{-| Returns a row at given index as a list.
Nothing if index is out of bounds.
-}
getRow : Int -> Matrix a -> Result String (Array a)
getRow y m =
    case A.get y m of
        Just x ->
            Ok x

        Nothing ->
            Err "Matrix: Row index out of bounds"


{-| Returns a column at given index as an array.
Noting if index is out of bounds.
-}
getColumn : Int -> Matrix a -> Result String (Array a)
getColumn x m =
    A.foldr
        (\row c -> A.push (A.get x row) c)
        A.empty
        m
        |> ama2maa


{-| Returns all elements of Matrix in a single array.
-}
toArray : Matrix a -> Array a
toArray =
    A.foldr A.append A.empty



{--
        Primer
--}


tm : Matrix ( Int, Int, String )
tm =
    generate 5
        10
        (\x y ->
            ( x
            , y
            , String.fromInt (x * y)
            )
        )


ama2maa : Array (Maybe a) -> Result String (Array a)
ama2maa =
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
