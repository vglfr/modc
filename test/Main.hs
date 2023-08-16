module Main where

import Test.Hspec (hspec)

import Compiler (testCompile, testConstify, testMainify, testVarify)
import VM (testSpool)

main :: IO ()
main = do
  hspec $ do
    testCompile
    testConstify
    testMainify
    testVarify
  hspec testSpool
