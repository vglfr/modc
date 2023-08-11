{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wno-missing-methods #-}

module Modc.VM where

import Data.List (elemIndex, nub, intercalate)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Data.Graph.Inductive (Adj, Context, Gr, buildGr, postorder, dff, labNodes, lab)
import Data.Graph.Inductive.Query.DFS (scc)
-- import Data.HashMap.Strict ((!?), HashMap, insert, lookup, member, size, toList)
import Data.HashMap.Strict ((!?), size, toList)

import Modc.AST
  (
    Comb ((:=), Fun)
  , Combs
  , Exp (Bin, Exe, Val, Var)
  , Id
  , Op
  , Prog (Prog)
  )
import Data.Char (isDigit)

-- type Data = HashMap Double Int
-- type BSS  = [String]
-- type Tape = (Text, Data, BSS, Name)

data Spool a = Spool Name [a]

data Label
  = Ass Name [Ins]
  | Pro Name [Ins]

type Name = String

data Ins
  = Two Op Val Val
  | Cal Id [Val]
  | Loa Val

data Val
  = Arg Int
  | Con Double
  | Ref Int
  | Sym Id
  -- | Map Int

instance Show a => Show (Spool a) where
  show (Spool n as) = intercalate "\n\n" $ n : fmap show as

instance Show Label where
  show (Ass n is) = intercalate "\n" $ n <> ":" : fmap (offset 2 . show) is
  show (Pro n is) = intercalate "\n" $ n <> "!" : fmap (offset 2 . show) is

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Cal i is) = unwords $ i : fmap show is
  show (Loa v) = show v

instance Show Val where
  show (Arg x) = "'" <> show x
  show (Con x) = show x
  show (Ref x) = "[" <> show x <> "]"
  show (Sym x) = x
  -- show (Map x) = "C" <> show x

spool :: Prog -> Spool Label
spool (Prog i cs) = let cs' = rebind cs
                        is = unwind . graph $ cs'
                     in Spool i (fmap (label cs') is)
 where
  label cs' i' = case cs' !? i' of
                   Just (_ := e) -> Ass i' (spool' e)
                   Just (Fun _ _ e) -> Pro i' (spool' e)

spool' :: Exp -> [Ins]
spool' = fmap translate . flat
 where
  translate e = case e of
                  Bin o a b -> Two o (val a) (val b)
                  Exe i as -> Cal i (fmap val as)
                  Val v -> Loa $ Con v
                  Var i -> Loa $ Sym i
  val :: Exp -> Val
  val e = case e of
            Val v -> Con v
            Var i -> if isDigit (head i) then Arg (read i) else Sym i
            _ -> Ref 0 -- (Ref $ index e es - index e' es, cs')

flat :: Exp -> [Exp]
flat e = case e of
           Bin _ a b -> filter (not . unary) (flat a <> flat b <> [e])
           Exe _ as -> filter (not . unary) (concatMap flat as <> [e])
           Val _ -> [e]
           Var _ -> [e]
 where
  unary e' = case e' of
               Val _ -> True
               Var _ -> True
               _ -> False

rebind :: Combs -> Combs
rebind = fmap rebind'
 where
  rebind' c = case c of
                Fun i ps e -> let ps' = fmap show [0..length ps - 1]
                               in Fun i ps' (rename (zip ps ps') e)
                _ -> c
  rename m e = case e of
                 Bin o a b -> Bin o (rename m a) (rename m b)
                 Exe i as -> Exe i $ fmap (rename m) as
                 Var v -> maybe e Var $ lookup v m
                 Val _ -> e

graph :: Combs -> Gr String String
graph cs = let (ks,vs) = unzip . toList $ cs
            in buildGr . foldr (acc ks) mempty . zip [0..size cs - 1] $ vs
 where
  acc :: [Id] -> (Int,Comb) -> [Context String String] -> [Context String String]
  acc is (n,c) a = case c of
                     i := e      -> ([],n,i,vars e is) : a
                     Fun i is' e -> ([],n,i,vars e (is <> is')) : a
  vars :: Exp -> [Id] -> Adj String
  vars e is = case e of
                Bin _ e1 e2  -> nub $ vars e1 is <> vars e2 is
                Var i -> pure $ edge i is
                Exe i is' -> edge i is : concatMap (flip vars is) is'
                Val _ -> mempty
  edge i is = case elemIndex i is of
                 Just n  -> (i,n)
                 Nothing -> error $ "undefined identifier " <> show i

unwind :: Gr String String -> [Id]
unwind g = case cycles of
             [] -> fmap (fromJust . lab g) . postorder . head . dff [main] $ g
             cs -> error $ "cyclic references: " <> unwords (fmap (intercalate "<>") cs)
 where
  cycles = fmap (fmap (fromJust . lab g)) . filter ((>1) . length) . scc $ g
  !main = case lookup "main" . fmap swap . labNodes $ g of
            Just x  -> x
            Nothing -> error "program must have main"

-- ## --

offset :: Int -> String -> String
offset n s = replicate n ' ' <> s

-- ungr :: Prog -> [Id]
-- ungr (Prog _ cs) = unwind . graph $ cs

-- rebind'' :: Prog -> Combs
-- rebind'' (Prog _ cs) = rebind cs

-- graph' :: Prog -> Gr String String
-- graph' (Prog _ cs) = graph cs

-- flat' :: String -> Prog -> [Exp]
-- flat' n (Prog _ cs) = flat . expr . fromJust $ cs !? n

-- expr :: Comb -> Exp
-- expr (_ := e) = e
-- expr (Fun _ _ e) = e
