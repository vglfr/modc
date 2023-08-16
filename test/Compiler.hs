module Compiler where

import Test.Hspec (Spec, describe, it, shouldBe)

import Modc.AST
  (
    Op (Add, Div, Mul, Sub)
  )
import Modc.Compiler
  (
    ASM (Global, Extern, Section)
  , Block (Text)
  , compile, constify, mainify, printf64, varify
  )
import Modc.Example
  (
    s3, s4, s5, s6, s7, s8, s9, s10, s13, s14, s15
  , s3', s4', s5', s6', s7', s8', s9', s10', s13', s14', s15'
  )
import Modc.VM
  (
    Ins (Cal, Loa, Two)
  , Label (Ass, Pro)
  , Spool (Spool)
  , Val (Arg, Ref, Sym)
  )
import Data.HashMap.Lazy (fromList)

testCompile :: Spec
testCompile = describe "Modc.Compiler" $ do
  it "compile s3" $ do
    compile s3 `shouldBe` Spool "p3"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 3.0"
          , "?1:         dq 2.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          ]
      , Section ".text"
          [
            printf64
      --     , "main:"
      --     , "        ; [it] <- [C0] * [C1]"
      --     , "        fld         qword [C0]"
      --     , "        fmul        qword [C1]"
      --     , "        fstp        qword [it]"
      --     , ""
      --     , "        ; printf_f64 [it]"
      --     , "        push        qword [it]"
      --     , "        call        printf_f64"
      --     , "        add         rsp, 8"
      --     , ""
      --     , "        ret"
          ]
      ]

  it "compile s4" $ do
    compile s4 `shouldBe` Spool "p4"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 3.0"
          , "?1:         dq 2.0"
          , "?2:         dq 4.0"
          , "?3:         dq 6.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s5" $ do
    compile s5 `shouldBe` Spool "p5"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 5.0"
          , "?1:         dq 2.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "x:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s6" $ do
    compile s6 `shouldBe` Spool "p6"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 5.0"
          , "?1:         dq 2.0"
          , "?2:         dq 1.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "x:          resq 1"
          , "y:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s7" $ do
    compile s7 `shouldBe` Spool "p7"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 2.0"
          , "?1:         dq 3.0"
          , "?2:         dq 1.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s8" $ do
    compile s8 `shouldBe` Spool "p8"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 2.0"
          , "?1:         dq 4.0"
          , "?2:         dq 3.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s9" $ do
    compile s9 `shouldBe` Spool "p9"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 5.0"
          , "?1:         dq 2.0"
          , "?2:         dq 3.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "y:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s10" $ do
    compile s10 `shouldBe` Spool "p10"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 5.0"
          , "?1:         dq 2.0"
          , "?2:         dq 3.0"
          , "?3:         dq 7.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "y:          resq 1"
          , "z:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s13" $ do
    compile s13 `shouldBe` Spool "p13"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 5.0"
          , "?1:         dq 2.0"
          , "?2:         dq 3.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "y:          resq 1"
          , "z:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s14" $ do
    compile s14 `shouldBe` Spool "p14"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 2.0"
          , "?1:         dq 3.0"
          , "?2:         dq 5.0"
          , "?3:         dq 8.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          , "a:          resq 1"
          , "b:          resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

  it "compile s15" $ do
    compile s15 `shouldBe` Spool "p15"
      [
        Global "main"
      , Extern "printf"
      , Section ".data" . pure . Text $
          [
            "?F:         db \"%.2f\", 10, 0"
          , "?0:         dq 3.0"
          ]
      , Section ".bss" . pure . Text $
          [
            "?R:         resq 1"
          ]
      , Section ".text"
          [
            printf64
          ]
      ]

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

testMainify :: Spec
testMainify = describe "Modc.Compiler" $ do
  let mainify' (Spool n ls) = Spool n (mainify . fst . constify $ ls)

  it "mainify s3" $ do
    mainify' s3 `shouldBe` s3'

  it "mainify s4" $ do
    mainify' s4 `shouldBe` s4'

  it "mainify s5" $ do
    mainify' s5 `shouldBe` s5'

  it "mainify s6" $ do
    mainify' s6 `shouldBe` s6'

  it "mainify s7" $ do
    mainify' s7 `shouldBe` s7'

  it "mainify s8" $ do
    mainify' s8 `shouldBe` s8'

  it "mainify s9" $ do
    mainify' s9 `shouldBe` s9'

  it "mainify s10" $ do
    mainify' s10 `shouldBe` s10'

  it "mainify s13" $ do
    mainify' s13 `shouldBe` s13'

  it "mainify s14" $ do
    mainify' s14 `shouldBe` s14'

  it "mainify s15" $ do
    mainify' s15 `shouldBe` s15'

testVarify :: Spec
testVarify = describe "Modc.Compiler" $ do
  let varify' (Spool _ ls) = varify ls

  it "varify s3" $ do
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
