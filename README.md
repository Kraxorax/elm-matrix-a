# Matrix a

`Matrix a` is data-structure of some width and height, containing elements of type `a` on `x` and `y` indexes. It's similar to a table, but rows and columns have no names.
It is not a mathematical matrix, as there are no operations between matrices.

Main library exposes `Matrix a` creation, traversal, and some manipulation functions.


## Creation

#### by repetition
Call `repeat` with width and height, and a value to be repeated.
```elm
Matrix.repeat : Int -> Int -> a -> Matrix a

myFourByThreeMatrixOfZeros = Matrix.repeat 4 3 0
-- 0 0 0 0
-- 0 0 0 0
-- 0 0 0 0
```

#### by generator function
Call `generate` with width and height, and a _generator function_ `: Int -> Int -> a`.
Generator function will be called for every element of matrix and will receive `x` and `y` of that element as params. It's return value will be put into matrix.
```elm
Matrix.generate : Int -> Int -> (Int -> Int -> a) -> Matrix a

multiplicationTable = Matrix.generate 10 10 (\x y -> (x + 1) * (y + 1))
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
```

## Traversal

Matrix library exposes common transversal functions `map`, `indexedMap`, `foldr`, `foldl`.
Some examples:
#### map
```elm
myMatrix_1 = Matrix.repeat 3 3 0
-- 0 0 0
-- 0 0 0
-- 0 0 0
Matrix.map (String.fromInt) myMatrix_1
--  "0" "0" "0"
--  "0" "0" "0"
--  "0" "0" "0"
```

#### indexedMap
```elm
myMatrix_2 = Matrix.repeat 3 3 2
-- 2 2 2
-- 2 2 2
-- 2 2 2
Matrix.indexedMap (\x y element -> (x + y) * element |> String.fromInt) myMatrix_2
--  "0" "2" "4"
--  "2" "4" "6"
--  "4" "6" "8"
```

#### folding
```elm
myMatrix_3 = Matrix.generate 4 4 (\x y -> (String.fromInt x) ++ (String.fromInt y))
--  "00"    "10"    "20"    "30"
--  "01"    "11"    "21"    "31"
--  "02"    "12"    "22"    "32"
--  "03"    "13"    "23"    "33"
Matrix.foldr (++) "" myMatrix_3
--  "00102030011121310212223203132333" 
Matrix.foldl (++) "" myMatrix_3
--  "33231303322212023121110130201000"

```