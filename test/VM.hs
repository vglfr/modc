module VM where

import Control.Exception (evaluate)
import Test.Hspec (Spec, describe, errorCall, it, shouldBe, shouldThrow)

import Modc.Example
  (
    p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15
  ,         s3, s4, s5, s6, s7, s8, s9, s10,           s13, s14, s15
  )
import Modc.VM (spool)

testSpool :: Spec
testSpool = describe "Modc.VM" $ do
  it "spool p1" $ do
    evaluate (seq (spool p1) undefined) `shouldThrow` errorCall "Prelude.undefined" -- deepseq

  it "spool p2" $ do
    evaluate (seq (spool p2) undefined) `shouldThrow` errorCall "Prelude.undefined"

  it "spool p3" $ do
    spool p3 `shouldBe` s3

  it "spool p4" $ do
    spool p4 `shouldBe` s4

  it "spool p5" $ do
    spool p5 `shouldBe` s5

  it "spool p6" $ do
    spool p6 `shouldBe` s6

  it "spool p7" $ do
    spool p7 `shouldBe` s7

  it "spool p8" $ do
    spool p8 `shouldBe` s8

  it "spool p9" $ do
    spool p9 `shouldBe` s9

  it "spool p10" $ do
    spool p10 `shouldBe` s10

  it "spool p11" $ do
    evaluate (seq (spool p11) undefined) `shouldThrow` errorCall "Prelude.undefined"

  it "spool p12" $ do
    evaluate (seq (spool p12) undefined) `shouldThrow` errorCall "Prelude.undefined"

  it "spool p13" $ do
    spool p13 `shouldBe` s13

  it "spool p14" $ do
    spool p14 `shouldBe` s14

  it "spool p15" $ do
    spool p15 `shouldBe` s15

