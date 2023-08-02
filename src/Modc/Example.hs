{-# LANGUAGE OverloadedStrings #-}

module Modc.Example where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.HashMap.Strict (empty, fromList)
import Modc.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Exe)
  , Id
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )

{- x -}
i1 :: Id
i1 = "x"

{- yY32_ -}
i2 :: Id
i2 = "yY32_"

{- 5 -}
v1 :: Exp
v1 = 5

{- 5.5 -}
v2 :: Exp
v2 = 5.5

{- -5 -}
v3 :: Exp
v3 = -5

{- x -}
w1 :: Exp
w1 = "x"

{- yY32_ -}
w2 :: Exp
w2 = "yY32_"

{- 1 + 2 -} -- 3
b1 :: Exp
b1 = Bin Add 1 2

{- 1 - 2 -} -- -1
b2 :: Exp
b2 = Bin Sub 1 2

{- 1 * 2 -} -- 2
b3 :: Exp
b3 = Bin Mul 1 2

{- 1 / 2 -} -- 0.5
b4 :: Exp
b4 = Bin Div 1 2

{- -1 + 2 -} -- 1
b5 :: Exp
b5 = Bin Add (-1) 2

{- 1 - -2 -} -- 3
b6 :: Exp
b6 = Bin Sub 1 (-2)

{- 5 + 1 - 3 + 6 * 2 / 4 -} -- 6
b7 :: Exp
b7 = Bin Add (Bin Sub (Bin Add 5 1) 3) (Bin Div (Bin Mul 6 2) 4)

{- 5 + f x 3 - 3 -}
b8 :: Exp
b8 = Bin Sub (Bin Add 5 (Exe "f" ("x" :| [3]))) 3

{- 5 + f x (3 - y) - 3 -}
b9 :: Exp
b9 = Bin Sub (Bin Add 5 (Exe "f" ("x" :| [Bin Sub 3 "y"]))) 3

{- x * 2 -}
b10 :: Exp
b10 = Bin Mul "x" 2

{- 5 + 1 - 3 + 6 -}
b11 :: Exp
b11 = Bin Add (Bin Sub (Bin Add 5 1) 3) 6

{- 5 + 1 - 3 -}
b12 :: Exp
b12 = Bin Sub (Bin Add 5 1) 3

{- f 5 -}
e1 :: Exp
e1 = Exe "f" (5 :| [])

{- f 5 3 -}
e2 :: Exp
e2 = Exe "f" (5 :| [3])

{- f (5 + 3) -}
e3 :: Exp
e3 = Exe "f" (Bin Add 5 3 :| [])

{- f 3 (5 + y) -}
e4 :: Exp
e4 = Exe "f" (3 :| [Bin Add 5 "y"])

{- f x y -}
e5 :: Exp
e5 = Exe "f" ("x" :| ["y"])

{- f 3 (5 + y) x -}
e6 :: Exp
e6 = Exe "f" (3 :| [Bin Add 5 "y", "x"])

{- x = 5 -}
a1 :: Comb
a1 = "x" := 5

{- x = 5 + 1 / 4 - 3 * 2 -}
a2 :: Comb
a2 = "x" := Bin Sub (Bin Add 5 (Bin Div 1 4)) (Bin Mul 3 2)

{- x = 5 + y -}
a3 :: Comb
a3 = "x" := Bin Add 5 "y"

{- x = 5 + f y -}
a4 :: Comb
a4 = "x" := Bin Add 5 (Exe "f" ("y" :| []))

{- main = 5 -} -- 5
a5 :: Comb
a5 = "main" := 5

{- f x = (x + x) * x -}
f1 :: Comb
f1 = Fun "f" ("x" :| []) (Bin Mul (Bin Add "x" "x") "x") empty

{- f x y = (x + y) * x / y -}
f2 :: Comb
f2 = Fun "f" ("x" :| ["y"]) (Bin Div (Bin Mul (Bin Add "x" "y") "x") "y") empty

{-
f x y = g x + z
 where
  g a = a / 2
  z = y * 2
-}
f3 :: Comb
f3 = Fun
       "f"
       ("x" :| ["y"])
       (Bin Add (Exe "g" ("x" :| [])) "z")
       (fromList [
                   ("g", Fun "g" ("a" :| []) (Bin Div "a" 2) empty)
                 , ("z", "z" := Bin Mul "y" 2)
                 ])

{-
x = 5
main = x * 2
-} -- 10
p1 :: Prog
p1 = Prog $ fromList
  [
    ("x", "x" := 5)
  , ("main", "main" := Bin Mul "x" 2)
  ]

{-
x = 5
y = x * 2
main = x + 1 - y
-} -- -4
p2 :: Prog
p2 = Prog $ fromList
  [
    ("x", "x" := 5)
  , ("y", "y" := Bin Mul "x" 2)
  , ("main", "main" := Bin Sub (Bin Add "x" 1) "y")
  ]

{-
y = 5
f x = y * 2 - x
main = f 3 - 2 * y
-} -- -3
p3 :: Prog
p3 = Prog $ fromList
  [
    ("y", "y" := 5)
  , ("f", Fun "f" ("x" :| []) (Bin Sub (Bin Mul "y" 2) "x") empty)
  , ("main", "main" := Bin Sub (Exe "f" (3 :| [])) (Bin Mul 2 "y"))
  ]

{-
z = 5

f x = z * 2 - x

u = 7 + f z

g x y = f y * h x * 3 / z - 2 * w
 where
  h x = f x / 3
  w = u + 2

main = f 3 - 2 * z + g u z - z
-} -- -38
p4 :: Prog
p4 = Prog $ fromList
  [
    ("z", "z" := 5)
  , ("f", Fun "f" ("x" :| []) (Bin Sub (Bin Mul "z" 2) "x") empty)
  , ("u", "u" := Bin Add 7 (Exe "f" ("z" :| [])))
  , (
      "g"
    , Fun
        "g"
        ("x" :| ["y"])
        (Bin Sub (Bin Div (Bin Mul (Bin Mul (Exe "f" ("y" :| [])) (Exe "h" ("x" :| []))) 3) "z") (Bin Mul 2 "w"))
        (fromList
           [
             ("h", Fun "h" ("x" :| []) (Bin Div (Exe "f" ("x" :| [])) 3) empty)
           , ("w", "w" := Bin Add "u" 2)
           ]
        )
    )
  , ("main", "main" := Bin Sub (Bin Add (Bin Sub (Exe "f" (3 :| [])) (Bin Mul 2 "z")) (Exe "g" ("u" :| ["z"]))) "z")
  ]

{-
main = 3 * 2
-} -- 6
p5 :: Prog
p5 = Prog $ fromList
  [
    ("main", "main" := Bin Mul 3 2)
  ]

{-
main = (3 + 2) / ((3 - 2) * 4 + 6)
-} -- 0.5
p6 :: Prog
p6 = Prog $ fromList
  [
    ("main", "main" := Bin Div (Bin Add 3 2) (Bin Add (Bin Mul (Bin Sub 3 2) 4) 6))
  ]

{-
f x = 2 - x
main = f 3 - 2
-} -- -3
p7 :: Prog
p7 = Prog $ fromList
  [
    ("f", Fun "f" ("x" :| []) (Bin Sub 2 "x") empty)
  , ("main", "main" := Bin Sub (Exe "f" (3 :| [])) 2)
  ]

{-
x = 5
y = 7 + 5 * x

f u w = g u * 3 / y - 2 * x + g w
 where
  g a = 5 * a / 3

main = f 3 y - 2 * x + f y 0 - y
-}
p8 :: Prog
p8 = Prog $ fromList
  [
    ("x", "x" := 5)
  , ("y", "y" := Bin Add 7 (Bin Mul 5 "x"))
  , (
      "f"
    , Fun
        "f"
        ("u" :| ["w"])
        (Bin Add (Bin Sub (Bin Div (Bin Mul (Exe "g" ("u" :| [])) 3) "y") (Bin Mul 2 "x")) (Exe "g" ("w" :| [])))
        (fromList
           [
             ("g", Fun "g" ("a" :| []) (Bin Div (Bin Mul 5 "a") 3) empty)
           ]
        )
    )
  , ("main", "main" := Bin Sub (Bin Add (Bin Sub (Exe "f" (3 :| ["y"])) (Bin Mul 2 "x")) (Exe "f" ("y" :| [0]))) "y")
  ]

{-
a = x - 3
b = 5 * a - a
c = d + 4 - x
d = a
x = 5
y = x * 2
z = x + 4 * y + b
main = x + 1 - y / a - 2 * z + b * 3 / c
-} -- -81
p9 :: Prog
p9 = Prog $ fromList
  [
    ("a", "a" := Bin Sub "x" 3)
  , ("b", "b" := Bin Sub (Bin Mul 5 "a") "a")
  , ("c", "c" := Bin Sub (Bin Add "d" 4) "x")
  , ("d", "d" := "a")
  , ("x", "x" := 5)
  , ("y", "y" := Bin Mul "x" 2)
  , ("z", "z" := Bin Add (Bin Add "x" (Bin Mul 4 "y")) "b")
  , ("main", "main" := Bin Add (Bin Sub (Bin Sub (Bin Add "x" 1) (Bin Div "y" "a")) (Bin Mul 2 "z")) (Bin Div (Bin Mul "b" 3) "c"))
  ]
