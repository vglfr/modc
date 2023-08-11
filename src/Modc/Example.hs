{-# LANGUAGE OverloadedStrings #-}

module Modc.Example where

import Data.HashMap.Strict (fromList)

import Modc.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Exe)
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )
-- import Modc.Compiler
--   (
--     Line
--   )
import Modc.VM
  (
    Ins (Two, Cal, Loa)
  , Label (Ass, Pro)
  , Spool (Spool)
  , Val (Arg, Con, Ref, Sym)
  )

{- _|_
-}
p1 :: Prog
p1 = Prog "p1" mempty

{- _|_
x = 3 * 2
-}
p2 :: Prog
p2 = Prog "p2" $ fromList
  [
    ("x", "x" := Bin Mul 3 2)
  ]

{- 6
main = 3 * 2
-}
p3 :: Prog
p3 = Prog "p3" $ fromList
  [
    ("main", "main" := Bin Mul 3 2)
  ]

s3 :: Spool Label
s3 = Spool "p3"
  [
    Ass "main"
      [
        Two Mul (Con 3) (Con 2)
      ]
  ]

{- 0.5
x = 5
main = (3 + 2) / ((3 - 2) * 4 + 6)
-}
p4 :: Prog
p4 = Prog "p4" $ fromList
  [
    ("x", "x" := 5)
  , ("main", "main" := Bin Div (Bin Add 3 2) (Bin Add (Bin Mul (Bin Sub 3 2) 4) 6))
  ]

s4 :: Spool Label
s4 = Spool "p4"
  [
    Ass "main"
      [
        Two Add (Con 3) (Con 2)
      , Two Sub (Con 3) (Con 2)
      , Two Mul (Ref (-1)) (Con 4)
      , Two Add (Ref (-1)) (Con 6)
      , Two Div (Ref (-4)) (Ref (-1))
      ]
  ]

{- 6
x = 5 - 2
main = x * 2
-}
p5 :: Prog
p5 = Prog "p5" $ fromList
  [
    ("x", "x" := Bin Sub 5 2)
  , ("main", "main" := Bin Mul "x" 2)
  ]

s5 :: Spool Label
s5 = Spool "p5"
  [
    Ass "x"
      [
        Two Sub (Con 5) (Con 2)
      ]
  , Ass "main"
      [
        Two Mul (Sym "x") (Con 2)
      ]
  ]

{- -4
x = 5
y = x * 2
main = x + 1 - y
-}
p6 :: Prog
p6 = Prog "p6" $ fromList
  [
    ("x", "x" := 5)
  , ("y", "y" := Bin Mul "x" 2)
  , ("main", "main" := Bin Sub (Bin Add "x" 1) "y")
  ]

s6 :: Spool Label
s6 = Spool "p6"
  [
    Ass "x"
      [
        Loa (Con 5)
      ]
  , Ass "y"
      [
        Two Mul (Sym "x") (Con 2)
      ]
  , Ass "main"
      [
        Two Add (Sym "x") (Con 1)
      , Two Sub (Ref (-1)) (Sym "y")
      ]
  ]

{- -2
f x = 2 - x
main = f 3 - 2 + f 1
-}
p7 :: Prog
p7 = Prog "p7" $ fromList
  [
    ("f", Fun "f" ["x"] (Bin Sub 2 "x"))
  , ("main", "main" := Bin Add (Bin Sub (Exe "f" [3]) 2) (Exe "f" [1]))
  ]

s7 :: Spool Label
s7 = Spool "p7"
  [
    Pro "f"
      [
        Two Sub (Con 2) (Arg 0)
      ]
  , Ass "main"
      [
        Cal "f" [Con 3]
      , Two Sub (Ref (-1)) (Con 2)
      , Cal "f" [Con 1]
      , Two Add (Ref (-2)) (Ref (-1))
      ]
  ]

{- -5
f x y = 2 - x + y
g y = y * 4
main = f 3 4 - g 2
-}
p8 :: Prog
p8 = Prog "p8" $ fromList
  [
    ("f", Fun "f" ["x", "y"] (Bin Add (Bin Sub 2 "x") "y"))
  , ("g", Fun "g" ["x"] (Bin Mul "x" 4))
  , ("main", "main" := Bin Sub (Exe "f" [3, 4]) (Exe "g" [2]))
  ]

s8 :: Spool Label
s8 = Spool "p8"
  [
    Pro "f"
      [
        Two Sub (Con 2) (Arg 0)
      , Two Add (Ref (-1)) (Arg 1)
      ]
  , Pro "g"
      [
        Two Mul (Arg 0) (Con 4)
      ]
  , Ass "main"
      [
        Cal "f" [Con 3, Con 4]
      , Cal "g" [Con 2]
      , Two Sub (Ref (-2)) (Ref (-1))
      ]
  ]

{- -3
y = 5
f x = y * 2 - x
main = f 3 - 2 * y
-}
p9 :: Prog
p9 = Prog "p9" $ fromList
  [
    ("y", "y" := 5)
  , ("f", Fun "f" ["x"] (Bin Sub (Bin Mul "y" 2) "x"))
  , ("main", "main" := Bin Sub (Exe "f" [3]) (Bin Mul 2 "y"))
  ]

s9 :: Spool Label
s9 = Spool "p9"
  [
    Ass "y"
      [
        Loa (Con 5)
      ]
  , Pro "f"
      [
        Two Mul (Sym "y") (Con 2)
      , Two Sub (Ref (-1)) (Arg 0)
      ]
  , Ass "main"
      [
        Cal "f" [Con 3]
      , Two Mul (Con 2) (Sym "y")
      , Two Sub (Ref (-2)) (Ref (-1))
      ]
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
p10 = Prog "p10" $ fromList
  [
    ("y", "y" := 5)
  , ("z", "z" := 3)
  , ("f", Fun "f" ["x"] (Bin Sub (Bin Mul "y" 2) "x"))
  , ("g", Fun "g" ["y", "u"] (Bin Add (Bin Add "z" (Bin Div "y" 3)) "u"))
  , ("main", "main" := Bin Add (Bin Sub (Exe "f" [3]) (Bin Mul 2 "y")) (Bin Div "z" (Exe "g" ["z", 7])))
  ]

s10 :: Spool Label
s10 = Spool "p10"
  [
    Ass "y"
      [
        Loa (Con 5)
      ]
  , Pro "f"
      [
        Two Mul (Sym "y") (Con 2)
      , Two Sub (Ref (-1)) (Arg 0)
      ]
  , Ass "z"
      [
        Loa (Con 3)
      ]
  , Pro "g"
      [
        Two Div (Arg 0) (Con 3)
      , Two Add (Sym "z") (Ref (-1))
      , Two Add (Ref (-1)) (Arg 1)
      ]
  , Ass "main"
      [
        Cal "f" [Con 3]
      , Two Mul (Con 2) (Sym "y")
      , Two Sub (Ref (-2)) (Ref (-1))
      , Cal "g" [Sym "z", Con 7]
      , Two Div (Sym "z") (Ref (-1))
      , Two Add (Ref (-3)) (Ref (-1))
      ]
  ]

{- _|_
main = x * 2
-}
p11 :: Prog
p11 = Prog "p11" $ fromList
  [
    ("main", "main" := Bin Mul "x" 2)
  ]

{- _|_
x = y
y = x
main = x * 2
-}
p12 :: Prog
p12 = Prog "p12" $ fromList
  [
    ("x", "x" := "y")
  , ("y", "y" := "x")
  , ("main", "main" := Bin Mul "x" 2)
  ]

{- -3
z = 3
y = 5
f x = y * 2 - x
main = f z - 2 * y
-}
p13 :: Prog
p13 = Prog "p13" $ fromList
  [
    ("z", "z" := 3)
  , ("y", "y" := 5)
  , ("f", Fun "f" ["x"] (Bin Sub (Bin Mul "y" 2) "x"))
  , ("main", "main" := Bin Sub (Exe "f" ["z"]) (Bin Mul 2 "y"))
  ]

s13 :: Spool Label
s13 = Spool "p13"
  [
    Ass "y"
      [
        Loa (Con 5)
      ]
  , Pro "f"
      [
        Two Mul (Sym "y") (Con 2)
      , Two Sub (Ref (-1)) (Arg 0)
      ]
  , Ass "z"
      [
        Loa (Con 3)
      ]
  , Ass "main"
      [
        Cal "f" [Sym "z"]
      , Two Mul (Con 2) (Sym "y")
      , Two Sub (Ref (-2)) (Ref (-1))
      ]
  ]

{- -2
a = 2 - 3
b = 5 * a - a
f x = a * x
g b c = a + b - c + 8
main = a - 2 + b * f 3 / g a (b * 2)
-}
p14 :: Prog
p14 = Prog "p14" $ fromList
  [
    ("a", "a" := Bin Sub 2 3)
  , ("b", "b" := Bin Sub (Bin Mul 5 "a") "a")
  , ("f", Fun "f" ["x"] (Bin Mul "a" "x"))
  , ("g", Fun "g" ["b", "c"] (Bin Add (Bin Sub (Bin Add "a" "b") "c") 8))
  , ("main", "main" := Bin Add (Bin Sub "a" 2) (Bin Div (Bin Mul "b" (Exe "f" [3])) (Exe "g" ["a", Bin Mul "b" 2])))
  ]

s14 :: Spool Label
s14 = Spool "p14"
  [
    Ass "a"
      [
        Two Sub (Con 2) (Con 3)
      ]
  , Pro "f"
      [
        Two Mul (Sym "a") (Arg 0)
      ]
  , Ass "b"
      [
        Two Mul (Con 5) (Sym "a")
      , Two Sub (Ref (-1)) (Sym "a")
      ]
  , Pro "g"
      [
        Two Add (Sym "a") (Arg 0)
      , Two Sub (Ref (-1)) (Arg 1)
      , Two Add (Ref (-1)) (Con 8)
      ]
  , Ass "main" -- a - 2,f 3,b * f 3,b * 2,g a (b * 2),b * f 3 / g a (b * 2),a - 2 + b * f 3 / g a (b * 2)
      [
        Two Sub (Sym "a") (Con 2)
      , Cal "f" [Con 3]
      , Two Mul (Sym "b") (Ref (-1))
      , Two Mul (Sym "b") (Con 2)
      , Cal "g" [Sym "a", Ref (-1)]
      , Two Div (Ref (-3)) (Ref (-1))
      , Two Add (Ref (-6)) (Ref (-1))
      ]
  ]

{- 3
main = 3
-}
p15 :: Prog
p15 = Prog "p15" $ fromList
  [
    ("main", "main" := 3)
  ]

s15 :: Spool Label
s15 = Spool "p15"
  [
    Ass "main"
      [
        Loa (Con 3)
      ]
  ]

-- l15 :: Spool Line
-- l15 = Spool "p15" undefined
