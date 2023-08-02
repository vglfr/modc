{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Modc.Compiler where

import Data.List (intercalate)
import Numeric (showHex)

import Data.HashMap.Strict (toList)
import Data.Hashable (hash)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

import Modc.AST
  (
    Op (Add, Div, Mul, Sub)
  , Id
  )
import Modc.VM
  (
    Cons
  , Ins (Loa, Sav, Two)
  , Tape
  , Val (Ref)
  , Vars
  )

type Module = (String, String)
type Line = String

section :: String -> [Line] -> [Line]
section s is = "section " <> s : fmap offset is
 where
  offset cs = if last cs == ':'
              then cs
              else "        " <> cs

data' :: Cons -> [Line]
data' cs = fmap (uncurry fconst) (toList cs)
 where
  fconst k v = "C" <> show v <> ":         dq " <> show k

bss :: Vars -> [Line]
bss vs = "RES:        resq 1" : fmap fvar vs
 where
  fvar v = v <> ":          resq 1"

text :: [Ins] -> [Line]
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

global :: String -> [Line]
global s = pure $ "global " <> s

extern :: String -> [Line]
extern s = pure $ "extern " <> s

printf64 :: Module
printf64 = ("_printf_f64",) . unlines . intercalate (pure mempty) $
  [
    global "_printf_f64"
  , extern "printf"
  , data''
  , text'
  ]
 where
  data'' =
    [
      "section .data"
    , "        FST:        db \"%.2f\", 10, 0"
    ]
  text' =
    [
      "section .text"
    , "_printf_f64:"
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

compile :: Tape -> (Id, [Module])
compile t@(_,_,_,i) = (i, [printf64, main t])

run :: (Id, [Module]) -> IO ()
run (s,ms) = do
  mapM_ (createDirectoryIfMissing True) [aDir, oDir]
  mapM_ (\(_,c) -> putStrLn c) ms
  mapM_ (\(n,c) -> writeFile (aDir <> n <> ".s") c) ms
  mapM_ (\(n,_) -> readProcess "nasm" ["-g", "-f", "elf64", aDir <> n <> ".s", "-o", oDir <> n <> ".o"] mempty) ms
  readProcess "gcc" (["-z", "noexecstack", "-o", hDir <> "a.out"] <> map (\(n,_) -> oDir <> n <> ".o") ms) mempty >>= putStrLn
  readProcess (hDir <> "a.out") mempty mempty >>= putStrLn
 where
  aDir = hDir <> "/asm/"
  oDir = hDir <> "/obj/"
  hDir = "/tmp/modc/" <> s <> "-" <> showHex (abs . hash . concatMap snd $ ms) mempty <> "/"
