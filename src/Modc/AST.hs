{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Modc.AST where

import Data.List (intercalate, intersperse, sortOn)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

import Data.HashMap.Strict (HashMap, elems)

data Prog = Prog Id Combs

type Combs = HashMap Id Comb
type Id = String

data Comb
  = Id := Exp
  | Fun Id [Id] Exp

data Exp
  = Bin Op Exp Exp
  | Exe Id [Exp]
  -- | Ter Cmp Exp Exp Exp Exp
  | Var Id
  | Val Double
  deriving Eq

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving Eq

-- data Cmp
--   = Eq
--   | Ne
--   | Gt
--   | Lt
--   | Gte
--   | Lte
--   deriving Eq

instance Show Prog where
  show (Prog _ cs) = intercalate "\n\n" . fmap show . sortOn name . elems $ cs -- graph
   where
    name c = case c of
               Fun i _ _ -> i
               "main" := _ -> "~"
               i := _ -> " " <> i

instance Show Comb where
  show (i := e) = i <> " = " <> show e
  show (Fun i as e) = i <> " " <> unwords as <> " = " <> show e

instance Show Exp where
  showsPrec n e = case e of
                    Bin o x y -> let m = prec o
                                  in showParen (n > m) $ showsPrec m x . shows o . showsPrec (m+1) y
                    Exe f es -> showString f . showSpace . showArgs es
                    -- Ter c a b x y -> undefined
                    Var i -> showString i
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
   where
    showArgs = foldr (.) id . intersperse showSpace . fmap showArg
    showArg a = case a of
                  Val _ -> shows a
                  Var _ -> shows a
                  _ -> showParen True (shows a)
    prec o = case o of
               Add -> 5
               Sub -> 5
               Mul -> 6
               Div -> 6

instance Show Op where
  show o = case o of
             Add -> " + "
             Sub -> " - "
             Mul -> " * "
             Div -> " / "

-- instance Show Cmp where
--   show o = case o of
--              Eq -> " == "
--              Ne -> " /= "
--              Gt -> " > "
--              Lt -> " < "
--              Gte -> " >= "
--              Lte -> " <= "

instance Num Exp where
  fromInteger = Val . fromInteger
  negate (Val n) = Val $ negate n

instance Fractional Exp where
  fromRational = Val . fromRational

instance IsString Exp where
  fromString = Var
