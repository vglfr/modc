module Modc.Util where

offseti :: String -> String
offseti s = if null s || last s == ':' then s else replicate 8 ' ' <> s

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s

spacen :: Int -> String
spacen n = replicate (10 - length (show n)) ' '

spaces :: String -> String
spaces s = replicate (11 - length s) ' '
