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

-- type Data = HashMap Double Int
-- type BSS  = [String]
-- type Tape = (Text, Data, BSS, Name)

data Spool a = Spool { name :: Name, content :: [a] } deriving Show

type Name = String
type Label = (Name,[Ins])

data Ins
  = Two Op Val Val
  | Loa Val
  | Sav String
  -- | Call String

data Val
  = Con Double
  | Ref Int
  | Bss String

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Loa v) = show v
  show (Sav v) = v

instance Show Val where
  show (Con x) = show x
  show (Ref x) = "[" <> show x <> "]"
  show (Bss x) = x

spool :: Prog -> Spool Label
spool (Prog i cs) = let is = unwind . graph $ cs
                     in Spool i (fmap f is)
 where
  f i' = (i',spool' . expr . fromJust $ cs !? i')

expr :: Comb -> Exp
expr (_ := e) = e
expr (Fun _ _ e) = e

main' :: Prog -> Exp
main' (Prog _ cs) = expr . fromJust $ cs !? "main"

spool' :: Exp -> [Ins]
spool' = fmap acc . flat
 where
  acc e = case e of
            -- Bin o a b -> let (v1, cs1) = val a cs' es e
            --                  (v2, cs2) = val b cs1 es e
            --               in (Two o v1 v2 : is, cs2, vs, mempty)
            -- Bin _ _ _ -> undefined
            -- Exe _ _ -> undefined
            Val v -> Loa $ Con v
            -- Var i -> Loa $ Bss i

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

{-
class Spool a where
  spool' :: a -> Label

instance Spool Comb where
  spool' (i := e) cs = let (is,cs',_,_) = spool' e cs
                        in if i == "main"
                           then (is,cs',mempty,mempty)
                           else (is <> [Sav i], cs', [i], mempty)

instance Spool Exp where
  spool' t cs = let es = flat t
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

-}

-- spool p@(Prog i c) = let cs' = flatten . graph $ p
--                       in foldr acc mempty cs'
 -- where
  -- acc s (is,cs,vs,_) = let (is',cs',vs',_) = flip spool' cs . fromJust $ c !? s
  --                       in (is' <> is,cs' <> cs,vs' <> vs,i)

-- graph' :: Prog -> Gr String String
-- graph' (Prog _ cs) = graph cs

graph :: Combs -> Gr String String
graph cs = let (ks,vs) = unzip . toList $ cs
            in buildGr . foldr (acc ks) mempty . zip [0..size cs - 1] $ vs
 where
  acc :: [Id] -> (Int,Comb) -> [Context String String] -> [Context String String]
  acc is (n,c) a = case c of
                     i := e      -> ([],n,i,vars e is) : a
                     Fun i is' e -> ([],n,i,vars e (is <> is')) : a -- bound v free name clash
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
             [] -> fmap (fromJust . lab g) . postorder . head . dff [main] $ g -- ass first
             cs -> error $ "cyclic references: " <> unwords (fmap (intercalate "<>") cs)
 where
  cycles = fmap (fmap (fromJust . lab g)) . filter ((>1) . length) . scc $ g
  !main = case lookup "main" . fmap swap . labNodes $ g of
            Just x  -> x
            Nothing -> error "program must have main"
