{-# LANGUAGE OverloadedStrings #-}

module Modc.Example where

import Data.List.NonEmpty as L (fromList, singleton)
import Data.HashMap.Strict as M (fromList)
import Modc.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Exe)
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )

{- _|_
-}
p1 :: Prog
p1 = Prog "p1" mempty

{- _|_
x = 3 * 2
-}
p2 :: Prog
p2 = Prog "p2" $ M.fromList
  [
    ("x", "x" := Bin Mul 3 2)
  ]

{- 6
main = 3 * 2
-}
p3 :: Prog
p3 = Prog "p3" $ M.fromList
  [
    ("main", "main" := Bin Mul 3 2)
  ]

{- 0.5
main = (3 + 2) / ((3 - 2) * 4 + 6)
-}
p4 :: Prog
p4 = Prog "p4" $ M.fromList
  [
    ("main", "main" := Bin Div (Bin Add 3 2) (Bin Add (Bin Mul (Bin Sub 3 2) 4) 6))
  ]

{- 10
x = 5
main = x * 2
-}
p5 :: Prog
p5 = Prog "p5" $ M.fromList
  [
    ("x", "x" := 5)
  , ("main", "main" := Bin Mul "x" 2)
  ]

{- -4
x = 5
y = x * 2
main = x + 1 - y
-}
p6 :: Prog
p6 = Prog "p6" $ M.fromList
  [
    ("x", "x" := 5)
  , ("y", "y" := Bin Mul "x" 2)
  , ("main", "main" := Bin Sub (Bin Add "x" 1) "y")
  ]

{- -3
f x = 2 - x
main = f 3 - 2
-}
p7 :: Prog
p7 = Prog "p7" $ M.fromList
  [
    ("f", Fun "f" (singleton "x") (Bin Sub 2 "x"))
  , ("main", "main" := Bin Sub (Exe "f" $ singleton 3) 2)
  ]

{- -5
f x y = 2 - x + y
g y = y * 4
main = f 3 4 - g 2
-}
p8 :: Prog
p8 = Prog "p8" $ M.fromList
  [
    ("f", Fun "f" (L.fromList ["x", "y"]) (Bin Add (Bin Sub 2 "x") "y"))
  , ("g", Fun "g" (singleton "x") (Bin Mul "x" 4))
  , ("main", "main" := Bin Sub (Exe "f" $ L.fromList [3, 4]) (Exe "g" $ singleton 2))
  ]

{- -3
y = 5
f x = y * 2 - x
main = f 3 - 2 * y
-}
p9 :: Prog
p9 = Prog "p9" $ M.fromList
  [
    ("y", "y" := 5)
  , ("f", Fun "f" (singleton "x") (Bin Sub (Bin Mul "y" 2) "x"))
  , ("main", "main" := Bin Sub (Exe "f" $ singleton 3) (Bin Mul 2 "y"))
  ]

{- -2.72...
y = 5
z = 3
f x = y * 2 - x
g y u = z + y / 3 + u
        3 + 3 / 3 + 7
        11
main = f 3 - 2 * y + z / g z 7
         7 - 2 * 5 + 3 / 11
         -3        + 3 / 11
         -2.72...
-}
p10 :: Prog
p10 = Prog "p10" $ M.fromList
  [
    ("y", "y" := 5)
  , ("z", "z" := 3)
  , ("f", Fun "f" (singleton "x") (Bin Sub (Bin Mul "y" 2) "x"))
  , ("g", Fun "g" (L.fromList ["y", "u"]) (Bin Add (Bin Add "z" (Bin Div "y" 3)) "u"))
  , ("main", "main" := Bin Add (Bin Sub (Exe "f" $ singleton 3) (Bin Mul 2 "y")) (Bin Div 3 (Exe "g" $ L.fromList ["z", 7])))
  ]
