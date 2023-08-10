{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Modc.Compiler where

import Data.List (intercalate)
import Numeric (showHex)

-- import Data.HashMap.Strict as M (HashMap, insert, lookup, toList, size)
import Data.HashMap.Strict as M (HashMap)
import Data.Hashable (hash)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

-- import Modc.AST
--   (
--     Op (Add, Div, Mul, Sub)
--   )

import Modc.VM
  (
    -- Ins (Loa, Sav, Two)
    Label
  -- , Name
  , Spool (Spool)
  -- , Ins (Two)
  -- , Val (Con)
  )

type Line = String
type Consts = HashMap Double Int

run :: Spool Line -> IO ()
run (Spool n ls) = do
  createDirectoryIfMissing True hDir
  putStrLn . unlines $ ls
  writeFile (hDir <> n <> ".s") . unlines $ ls
  readProcess "nasm" ["-g", "-f", "elf64", hDir <> n <> ".s", "-o", hDir <> n <> ".o"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", hDir <> "a.out", hDir <> n <> ".o"] mempty >>= putStrLn
  readProcess (hDir <> "a.out") mempty mempty >>= putStrLn
 where
  hDir = "/tmp/modc/" <> n <> "-" <> showHex (abs . hash $ ls) mempty <> "/"

compile :: Spool Label -> Spool Line
compile (Spool n ls) = Spool n . intercalate [""] $
     global
  <> extern
  <> data' ls
  -- <> bss ls
  -- <> concatMap text ls
  -- <> printf64

global :: [[Line]]
global = pure $ pure "global main"

extern :: [[Line]]
extern = pure $ pure "extern printf"

data' :: [Label] -> [[Line]]
data' = undefined
-- data' ls = pure $
--   [
--     "section .data"
--   ] <> fmap fconst (consts ls)
--  where
--   fconst (k,v) = "C" <> show v <> ":         dq " <> show k
--   consts = toList . foldr (\l a -> foldr collect a $ snd l) mempty
--   collect i m = case i of
--                   Two _ (Con v1) (Con v2) -> upsert v2 (upsert v1 m)
--                   _ -> m
--   upsert k m = maybe (insert k (size m) m) (const m) $ M.lookup k m

constify :: [Label] -> ([Label], Consts)
constify = undefined

bss :: [Label] -> [[Line]]
bss = undefined
-- bss ls = pure
--   [
--     "section .bss"
--   ] <> it <> (fmap fvar . filter (/= "main") . fmap fst $ ls) -- drop Fun
--  where
--   it = pure ["RES:        resq 1"]
--   fvar v = pure $ v <> ":          resq 1"

text :: Label -> [[Line]]
text _ = pure
  [
    "section .text"
  ]
-- for each Ins make Line
  -- create bss  section from [Ins]
  -- create data section from [Ins]
-- add printf_f64

printf64 :: [[Line]]
printf64 = pure
  [
    "printf_f64:"
  , "        push        rbp"
  , "        mov         rbp, rsp"
  , ""
  , "        mov         rdi, FST"
  , "        mov         rax, 1"
  , "        movsd       xmm0, qword [rbp+16]"
  , "        call        printf"
  , ""
  , "        pop         rbp"
  , "        xor         rax, rax"
  , "        ret"
  ]

{-
section :: String -> [Line] -> [Line]
section s is = "section " <> s : fmap offset is
 where
  offset cs = if last cs == ':'
              then cs
              else "        " <> cs

text :: Text -> [Line]
text is = main' <> concatMap block is
 where
  block i = comment (show i) : instr i
  instr (Two o a b)
    | notRef a && notRef b = [fld a, op1 o b]
    | notRef a = [fld a, fxch, op0 o]
    | notRef b = pure $ op1 o b
    | ref a < ref b = [op0 o]
    | ref a > ref b = [fxch, op0 o]
  instr (Loa c) = pure $ fld c
  instr (Sav v) = pure ("fstp        qword [" <> v <> "]")
  notRef v = case v of
               Ref _ -> False
               _ -> True
  ref (Ref x) = x
  fld v = "fld         qword [" <> show v <> "]"
  op1 o v = case o of
              Add -> "fadd        qword [" <> show v <> "]"
              Sub -> "fsub        qword [" <> show v <> "]"
              Mul -> "fmul        qword [" <> show v <> "]"
              Div -> "fdiv        qword [" <> show v <> "]"
  fxch = "fxch"
  op0 o = case o of
            Add -> "faddp"
            Sub -> "fsubp"
            Mul -> "fmulp"
            Div -> "fdivp"
  main' = pure "main:"

comment :: String -> Line
comment s = "; " <> s

main :: Tape -> Module
main (is,cs,vs,_) = ("main",) . unlines . intercalate (pure mempty) $
  [
    global "main"
  , extern "_printf_f64"
  , section ".data" (data' cs)
  , section ".bss"  (bss vs)
  , section ".text" (text is) <> print'
  ]
 where
  print' =
    [
      ""
    , "        fstp        qword [RES]"
    , "        mov         rax, [RES]"
    , ""
    , "        push        rax"
    , "        call        _printf_f64"
    , ""
    , "        add         rsp, 8"
    , "        ret"
    ]
-}
