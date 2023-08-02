{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Modc.AST where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, intersperse, toList)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

import Data.HashMap.Strict (HashMap, elems)

data Prog = Prog Id Context

type Context = HashMap Id Comb
type Id = String

data Comb
  = Id := Exp
  | Fun Id (NonEmpty Id) Exp

data Exp
  = Bin Op Exp Exp
  | Exe Id (NonEmpty Exp)
  | Var Id
  | Val Double
  deriving Eq

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving Eq

instance Show Prog where
  show (Prog _ c) = intercalate "\n\n" . fmap show . elems $ c

instance Show Comb where
  show (i := e) = i <> " = " <> show e
  show (Fun i as e) = i <> " " <> (unwords . toList $ as) <> " = " <> show e

instance Show Exp where
  showsPrec n e = case e of
                    Bin o x y -> let m = prec o
                                  in showParen (n > m) $ showsPrec m x . shows o . showsPrec (m+1) y
                    Exe f es -> showString f . showSpace . showArgs es
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

instance Num Exp where
  fromInteger = Val . fromInteger
  negate (Val n) = Val $ negate n

instance Fractional Exp where
  fromRational = Val . fromRational

instance IsString Exp where
  fromString = Var
