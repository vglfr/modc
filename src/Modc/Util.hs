module Modc.Util where

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s
