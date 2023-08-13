module Modc.Util where

offsets :: String -> String
offsets s = if null s || last s == ':' then s else replicate 8 ' ' <> s

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s
