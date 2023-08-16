{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Modc.Compiler where

import Data.Foldable (foldl')
import Data.List (intercalate)
import Numeric (showHex)

import Data.HashMap.Strict as M (HashMap, insert, lookup, toList, size)
-- import Data.HashMap.Strict (HashMap, toList)
import Data.Hashable (hash)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

-- import Modc.AST
--   (
--     Op (Add, Div, Mul, Sub)
--   )
import Modc.Util (offsets)
import Modc.VM
  (
    Ins (Cal, Loa, Two)
  , Label (Ass, Pro)
  , Name
  , Spool (Spool)
  , Val (Con, Sym)
  )

data Section
  = Global String
  | Extern String
  | Section String [Line]

type Line = String
type Consts = HashMap Double Int

instance Show Section where
  show (Global n) = "global " <> n
  show (Extern n) = "extern " <> n
  show (Section n ls) = "section " <> n <> "\n" <> intercalate "\n" (fmap offsets ls)

run :: Spool Line -> IO ()
run s@(Spool n ls) = do
  createDirectoryIfMissing True hDir
  print s
  writeFile (hDir <> n <> ".s") . unlines $ ls
  readProcess "nasm" ["-g", "-f", "elf64", hDir <> n <> ".s", "-o", hDir <> n <> ".o"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", hDir <> "a.out", hDir <> n <> ".o"] mempty >>= putStrLn
  readProcess (hDir <> "a.out") mempty mempty >>= putStrLn
 where
  hDir = "/tmp/modc/" <> n <> "-" <> showHex (abs . hash $ ls) mempty <> "/"

compile :: Spool Label -> Spool Section
compile (Spool n ls) = let (_ls',cs) = constify ls
                           vs = varify ls
                           -- ls'' = mainify ls'
                        in Spool n
                             [
                               global
                             , extern
                             , data' cs
                             , bss vs
                             -- , text ls''
                             ]

-- constify' :: Spool Label -> ([Label], Consts)
-- constify' (Spool _ ls) = constify ls

constify :: [Label] -> ([Label], Consts)
constify = foldl' clabel mempty
 where
  -- clabel :: ([Label], Consts) -> Label -> ([Label], Consts)
  clabel (ls,cs) l = case l of
                       Ass n is -> let (is',cs') = foldl' cins (mempty, cs) is
                                    in (ls <> [Ass n is'], cs')
                       Pro n is -> let (is',cs') = foldl' cins (mempty, cs) is
                                    in (ls <> [Pro n is'], cs')
  -- cins :: ([Ins], Consts) -> Ins -> ([Ins], Consts)
  cins (is,cs) i = case i of
                     Two n v1 v2 -> let (v1',cs' ) = cval v1 cs
                                        (v2',cs'') = cval v2 cs'
                                     in (is <> [Two n v1' v2'], cs'')
                     Cal n as -> let (as',cs') = foldl' cvals (mempty, cs) as
                                  in (is <> [Cal n (reverse as')], cs')
                     Loa v -> let (v',cs') = cval v cs
                               in (is <> [Loa v'], cs')
  -- cvals :: ([Val], Consts) -> Val -> ([Val], Consts)
  cvals (vs,cs) v = let (v',cs') = cval v cs
                     in (v' : vs, cs')
  -- cval :: Val -> Consts -> (Val, Consts)
  cval v m = case v of
               Con c -> let (n,m') = upsert c m
                         in (Sym $ '?' : show n, m')
               _ -> (v,m)
  -- upsert :: Double -> Consts -> (Int, Consts)
  upsert k m = maybe (size m, insert k (size m) m) (,m) $ M.lookup k m

varify :: [Label] -> [Name]
varify = concatMap vlabel
 where
  vlabel (Ass n _) = if n == "main" then mempty else pure n
  vlabel _ = mempty

mainify :: [Label] -> [Label]
mainify = undefined

global :: Section
global = Global "main"

extern :: Section
extern = Extern "printf"

data' :: Consts -> Section
data' cs = Section ".data" $
  "?F:          db \"%.2f\", 10, 0" : fmap (uncurry fconst) (toList cs)
 where
  fconst k v = show v <> ":         dq " <> show k

bss :: [Name] -> Section
bss vs = Section ".bss" $
  "?R:         resq 1" : fmap fvar vs
 where
  fvar v = v <> ":          resq 1"

text :: [Label] -> Section
text _ls = Section ".text"
  [
    -- "section .text"
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
