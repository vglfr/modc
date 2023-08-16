module Compiler where

import Test.Hspec (Spec, describe, it, shouldBe)

import Modc.AST
  (
    Op (Add, Div, Mul, Sub)
  )
import Modc.Compiler (constify, varify)
import Modc.Example
  (
    s3, s4, s5, s6, s7, s8, s9, s10, s13, s14, s15
  )
import Modc.VM
  (
    Label (Ass, Pro)
  , Ins (Cal, Loa, Two)
  , Spool (Spool)
  , Val (Arg, Ref, Sym)
  )
import Data.HashMap.Lazy (fromList)

testConstify :: Spec
testConstify = describe "Modc.Compiler" $ do
  let constify' (Spool _ ls) = constify ls

  it "constify s3" $ do
    constify' s3 `shouldBe`
      (
        [
          Ass "main"
            [
              Two Mul (Sym "?0") (Sym "?1")
            ]
        ]
      , fromList
        [
          (3.0, 0)
        , (2.0, 1)
        ]
      )

  it "constify s4" $ do
    constify' s4 `shouldBe`
      (
        [
          Ass "main"
            [
              Two Add (Sym "?0") (Sym "?1")
            , Two Sub (Sym "?0") (Sym "?1")
            , Two Mul (Ref (-1)) (Sym "?2")
            , Two Add (Ref (-1)) (Sym "?3")
            , Two Div (Ref (-4)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (3.0, 0)
        , (2.0, 1)
        , (4.0, 2)
        , (6.0, 3)
        ]
      )

  it "constify s5" $ do
    constify' s5 `shouldBe`
      (
        [
          Ass "x"
            [
              Two Sub (Sym "?0") (Sym "?1")
            ]
        , Ass "main"
            [
              Two Mul (Sym "x") (Sym "?1")
            ]
        ]
      , fromList
        [
          (5.0, 0)
        , (2.0, 1)
        ]
      )

  it "constify s6" $ do
    constify' s6 `shouldBe`
      (
        [
          Ass "x"
            [
              Loa (Sym "?0")
            ]
        , Ass "y"
            [
              Two Mul (Sym "x") (Sym "?1")
            ]
        , Ass "main"
            [
              Two Add (Sym "x") (Sym "?2")
            , Two Sub (Ref (-1)) (Sym "y")
            ]
        ]
      , fromList
        [
          (5.0, 0)
        , (2.0, 1)
        , (1.0, 2)
        ]
      )

  it "constify s7" $ do
    constify' s7 `shouldBe`
      (
        [
          Pro "f"
            [
              Two Sub (Sym "?0") (Arg 0)
            ]
        , Ass "main"
            [
              Cal "f" [Sym "?1"]
            , Two Sub (Ref (-1)) (Sym "?0")
            , Cal "f" [Sym "?2"]
            , Two Add (Ref (-2)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (2.0, 0)
        , (3.0, 1)
        , (1.0, 2)
        ]
      )

  it "constify s8" $ do
    constify' s8 `shouldBe`
      (
        [
          Pro "f"
            [
              Two Sub (Sym "?0") (Arg 0)
            , Two Add (Ref (-1)) (Arg 1)
            ]
        , Pro "g"
            [
              Two Mul (Arg 0) (Sym "?1")
            ]
        , Ass "main"
            [
              Cal "f" [Sym "?2", Sym "?1"]
            , Cal "g" [Sym "?0"]
            , Two Sub (Ref (-2)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (2.0, 0)
        , (4.0, 1)
        , (3.0, 2)
        ]
      )

  it "constify s9" $ do
    constify' s9 `shouldBe`
      (
        [
          Ass "y"
            [
              Loa (Sym "?0")
            ]
        , Pro "f"
            [
              Two Mul (Sym "y") (Sym "?1")
            , Two Sub (Ref (-1)) (Arg 0)
            ]
        , Ass "main"
            [
              Cal "f" [Sym "?2"]
            , Two Mul (Sym "?1") (Sym "y")
            , Two Sub (Ref (-2)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (5.0, 0)
        , (2.0, 1)
        , (3.0, 2)
        ]
      )

  it "constify s10" $ do
    constify' s10 `shouldBe`
      (
        [
          Ass "y"
            [
              Loa (Sym "?0")
            ]
        , Pro "f"
            [
              Two Mul (Sym "y") (Sym "?1")
            , Two Sub (Ref (-1)) (Arg 0)
            ]
        , Ass "z"
            [
              Loa (Sym "?2")
            ]
        , Pro "g"
            [
              Two Div (Arg 0) (Sym "?2")
            , Two Add (Sym "z") (Ref (-1))
            , Two Add (Ref (-1)) (Arg 1)
            ]
        , Ass "main"
            [
              Cal "f" [Sym "?2"]
            , Two Mul (Sym "?1") (Sym "y")
            , Two Sub (Ref (-2)) (Ref (-1))
            , Cal "g" [Sym "z", Sym "?3"]
            , Two Div (Sym "z") (Ref (-1))
            , Two Add (Ref (-3)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (5.0, 0)
        , (2.0, 1)
        , (3.0, 2)
        , (7.0, 3)
        ]
      )

  it "constify s13" $ do
    constify' s13 `shouldBe`
      (
        [
          Ass "y"
            [
              Loa (Sym "?0")
            ]
        , Pro "f"
            [
              Two Mul (Sym "y") (Sym "?1")
            , Two Sub (Ref (-1)) (Arg 0)
            ]
        , Ass "z"
            [
              Loa (Sym "?2")
            ]
        , Ass "main"
            [
              Cal "f" [Sym "z"]
            , Two Mul (Sym "?1") (Sym "y")
            , Two Sub (Ref (-2)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (5.0, 0)
        , (2.0, 1)
        , (3.0, 2)
        ]
      )

  it "constify s14" $ do
    constify' s14 `shouldBe`
      (
        [
          Ass "a"
            [
              Two Sub (Sym "?0") (Sym "?1")
            ]
        , Pro "f"
            [
              Two Mul (Sym "a") (Arg 0)
            ]
        , Ass "b"
            [
              Two Mul (Sym "?2") (Sym "a")
            , Two Sub (Ref (-1)) (Sym "a")
            ]
        , Pro "g"
            [
              Two Add (Sym "a") (Arg 0)
            , Two Sub (Ref (-1)) (Arg 1)
            , Two Add (Ref (-1)) (Sym "?3")
            ]
        , Ass "main"
            [
              Two Sub (Sym "a") (Sym "?0")
            , Cal "f" [Sym "?1"]
            , Two Mul (Sym "b") (Ref (-1))
            , Two Mul (Sym "b") (Sym "?0")
            , Cal "g" [Sym "a", Ref (-1)]
            , Two Div (Ref (-3)) (Ref (-1))
            , Two Add (Ref (-6)) (Ref (-1))
            ]
        ]
      , fromList
        [
          (2.0, 0)
        , (3.0, 1)
        , (5.0, 2)
        , (8.0, 3)
        ]
      )

  it "constify s15" $ do
    constify' s15 `shouldBe`
      (
        [
          Ass "main"
            [
              Loa (Sym "?0")
            ]
        ]
      , fromList
        [
          (3.0, 0)
        ]
      )

testVarify :: Spec
testVarify = describe "Modc.Compiler" $ do
  let varify' (Spool _ ls) = varify ls

  it "constify s3" $ do
    varify' s3 `shouldBe` []

  it "varify s4" $ do
    varify' s4 `shouldBe` []

  it "varify s5" $ do
    varify' s5 `shouldBe` ["x"]

  it "varify s6" $ do
    varify' s6 `shouldBe` ["x","y"]

  it "varify s7" $ do
    varify' s7 `shouldBe` []

  it "varify s8" $ do
    varify' s8 `shouldBe` []

  it "varify s9" $ do
    varify' s9 `shouldBe` ["y"]

  it "varify s10" $ do
    varify' s10 `shouldBe` ["y","z"]

  it "varify s13" $ do
    varify' s13 `shouldBe` ["y","z"]

  it "varify s14" $ do
    varify' s14 `shouldBe` ["a","b"]

  it "varify s15" $ do
    varify' s15 `shouldBe` []
