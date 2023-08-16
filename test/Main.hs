module Main where

import Test.Hspec (hspec)

import Compiler (testConstify, testVarify)
import VM (testSpool)

main :: IO ()
main = do
  hspec $ do
    testConstify
    testVarify
  hspec testSpool
