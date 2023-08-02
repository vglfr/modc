{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Modc.VM where

import Data.List (elemIndex, lookup, nub)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Data.Graph.Inductive (Adj, Context, Gr, buildGr, postorder, dff, labNodes, lab)
import Data.HashMap.Strict ((!?), HashMap, insert, lookup, size, toList)

import Modc.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Val, Var)
  , Id
  , Op
  , Prog (Prog)
  )

type Data = HashMap Double Int
type BSS  = [String]
type Text = [Ins]
type Name = String

type Tape = (Text, Data, BSS, Name)

-- data Tape = Tape
--   {
--     t :: Text
--   , d :: Data
--   , b :: BSS
--   , n :: Name
--   }

data Ins
  = Two Op Val Val
  | Loa Val
  | Sav String

data Val
  = Con Int
  | Ref Int
  | Tem String

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Loa v) = show v
  show (Sav v) = v

instance Show Val where
  show (Con x) = "C" <> show x
  show (Ref x) = "[" <> show x <> "]"
  show (Tem x) = x

class Spool a where
  spool :: Data -> a -> Tape

instance Spool Prog where
  spool _ p@(Prog i c) = let cs' = flatten . graph $ p
                          in foldr acc mempty cs'
   where
    acc s (is,cs,vs,_) = let (is',cs',vs',_) = spool cs . fromJust $ c !? s
                          in (is' <> is,cs' <> cs,vs' <> vs,i)

instance Spool Comb where
  spool cs (i := e) = let (is,cs',_,_) = spool cs e
                       in if i == "main"
                          then (is,cs',mempty,mempty)
                          else (is <> [Sav i], cs', [i], mempty)

instance Spool Exp where
  spool cs t = let es = flat t
                in foldr (acc es) (mempty,cs,mempty,mempty) es
   where
    flat e = case e of
               Bin _ a b -> flat' a <> flat' b <> [e]
               Val _ -> [e]
               Var _ -> [e]
               _ -> mempty
    flat' e = case e of
                Bin {} -> flat e
                _ -> mempty
    acc es e (is,cs',vs,_) = case e of
                               Bin o a b -> let (v1, cs1) = val a cs' es e
                                                (v2, cs2) = val b cs1 es e
                                             in (Two o v1 v2 : is, cs2, vs, mempty)
                               Val v -> let (c, cs1) = lookupd v cs'
                                         in (Loa (Con c) : is, cs1, vs, mempty)
                               Var i -> (Loa (Tem i) : is, cs', vs, mempty)
                               _ -> undefined
    val e cs' es e' = case e of
                        Val v -> let (c, cs1) = lookupd v cs'
                                  in (Con c, cs1)
                        Var i -> (Tem i, cs')
                        _ -> (Ref $ index e es - index e' es, cs')
    index e es = fromJust $ elemIndex e es
    lookupd k m = case Data.HashMap.Strict.lookup k m of
                    Just v  -> (v, m)
                    Nothing -> let s = size m in (s, insert k s m)

graph :: Prog -> Gr String String
graph (Prog _ cs) = let (ks,vs) = unzip . toList $ cs
                     in buildGr . foldr (acc ks) mempty . zip [0..size cs - 1] $ vs
 where
  acc :: [Id] -> (Int,Comb) -> [Context String String] -> [Context String String]
  acc is (n,c) a = case c of
                     i := e    -> ([],n,i,vars e is) : a
                     Fun i _ e -> ([],n,i,vars e is) : a
  vars :: Exp -> [Id] -> Adj String
  vars e is = case e of
                Bin _ e1 e2  -> nub $ vars e1 is <> vars e2 is
                Var i -> [(i,fromJust $ elemIndex i is)]
                Val _ -> mempty
  -- throw "undefined reference" on lookup error

flatten :: Gr String String -> [String]
flatten g = fmap (fromJust . lab g) . postorder . head . dff [main''] $ g
 where
  main'' = fromJust . Data.List.lookup "main" . fmap swap . labNodes $ g
  -- throw "cyclic reference" on bidirectioned edge
