module Modc.VM where

import Data.Char (isDigit)
import Data.List (elemIndex, nub, intercalate)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Data.Graph.Inductive (Adj, Context, Gr, buildGr, postorder, dff, labNodes, lab)
import Data.Graph.Inductive.Query.DFS (scc)
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
import Modc.Util (offset)

data Spool a = Spool Name [a] deriving Eq

data IR
  = Ass Name [Ins]
  | Pro Name [Ins]
  deriving Eq

type Name = String

data Ins
  = Two Op Val Val
  | Cal Id [Val]
  | Loa Val
  | Sav Val
  deriving Eq

data Val
  = Arg Int
  | Con Double
  | Ref Int
  | Sym Id
  deriving Eq

instance Show a => Show (Spool a) where
  show (Spool n as) = intercalate "\n\n" $ n : fmap show as

instance Show IR where
  show (Ass n is) = intercalate "\n" $ n <> ":" : fmap (offset 2 . show) is
  show (Pro n is) = intercalate "\n" $ n <> "!" : fmap (offset 2 . show) is

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Cal i is) = unwords $ i : fmap show is
  show (Loa v) = show v
  show (Sav v) = "~" <> show v

instance Show Val where
  show (Arg x) = "'" <> show x
  show (Con x) = show x
  show (Ref x) = "[" <> show x <> "]"
  show (Sym x) = x

spool :: Prog -> Spool IR
spool (Prog i cs) = let cs' = rebind cs
                        is = unwind . graph $ cs'
                     in Spool i (fmap (label cs') is)
 where
  label cs' i' = case cs' !? i' of
                   Just (_ := e) -> Ass i' (spool' e)
                   Just (Fun _ _ e) -> Pro i' (spool' e)
                   Nothing -> error "unreachable"

spool' :: Exp -> [Ins]
spool' es = fmap translate es'
 where
  translate e = case e of
                  Bin o a b -> Two o (val e a) (val e b)
                  Exe i as -> Cal i (fmap (val e) as)
                  Val v -> Loa $ Con v
                  Var i -> Loa $ Sym i
  val e e' = case e' of
               Val v -> Con v
               Var i -> if isDigit (head i) then Arg (read i) else Sym i
               _ -> Ref $ index e' - index e
  index e = fromJust $ elemIndex e es'
  es' = flat es

rebind :: Combs -> Combs
rebind = fmap bind
 where
  bind c = case c of
             Fun i ps e -> let ps' = fmap show [0..length ps - 1]
                            in Fun i ps' (rename (zip ps ps') e)
             _ -> c
  rename m e = case e of
                 Bin o a b -> Bin o (rename m a) (rename m b)
                 Exe i as -> Exe i $ fmap (rename m) as -- length as == length ps
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
