module Main where

import Test.Hspec (hspec)

import VM (testSpool)

main :: IO ()
main = hspec testSpool
