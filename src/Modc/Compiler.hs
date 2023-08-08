{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Modc.Compiler where

import Data.List (intercalate)
import Numeric (showHex)

-- import Data.HashMap.Strict (toList)
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
  , Spool (Spool, content, name)
  -- , Val (Ref)
  )

type Line = String

run :: Spool Line -> IO ()
run s = do
  createDirectoryIfMissing True hDir
  putStrLn . unlines $ content s
  writeFile (hDir <> name s <> ".s") . unlines $ content s
  readProcess "nasm" ["-g", "-f", "elf64", hDir <> name s <> ".s", "-o", hDir <> name s <> ".o"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", hDir <> "a.out", hDir <> name s <> ".o"] mempty >>= putStrLn
  readProcess (hDir <> "a.out") mempty mempty >>= putStrLn
 where
  hDir = "/tmp/modc/" <> name s <> "-" <> showHex (abs . hash $ content s) mempty <> "/"

compile :: Spool Label -> Spool Line
compile (Spool n ls) = Spool n $ intercalate [""]
  (
     global
  <> extern
  <> data' ls
  <> bss ls
  <> concatMap text ls
  <> printf64
  )

global :: [[Line]]
global = pure $ pure "global main"

extern :: [[Line]]
extern = pure $ pure "extern printf"

data' :: [Label] -> [[Line]]
data' _ = pure
  [
    "section .data"
  ]

-- data' :: Data -> [Line]
-- data' cs = fmap (uncurry fconst) (toList cs)
--  where
--   fconst k v = "C" <> show v <> ":         dq " <> show k

bss :: [Label] -> [[Line]]
bss _ = pure
  [
    "section .bss"
  ]

-- bss :: BSS -> [Line]
-- bss vs = "RES:        resq 1" : fmap fvar vs
--  where
--   fvar v = v <> ":          resq 1"

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
