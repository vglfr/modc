{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Modc.AST where

import Prelude hiding (unwords)

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, intersperse)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

import Data.HashMap.Strict (HashMap, elems)
import Data.Hashable (Hashable, hash, hashWithSalt)

newtype Prog = Prog Context deriving Eq

type Context = HashMap Id Comb

data Comb
  = Id := Exp
  | Fun Id (NonEmpty Id) Exp Context
  deriving Eq

newtype Id = Id String deriving Eq

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
  show (Prog c) = intercalate "\n\n" . fmap show . elems $ c

instance Show Comb where
  show (i := e) = show i <> " = " <> show e
  show (Fun i as e c) = show i <> " " <> unwords (fmap show as) <> " = " <> show e <> showCombs
   where showCombs = if null c
                     then ""
                     else "\n where\n  " <> intercalate "\n  " (fmap show . elems $ c)

instance Show Id where
  show (Id s) = s

instance Show Exp where
  showsPrec n e = case e of
                    Bin o x y -> let m = prec o
                                  in showParen (n > m) $ showsPrec m x . shows o . showsPrec (m+1) y
                    Exe f es -> shows f . showSpace . showArgs es
                    Var i -> shows i
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
  fromString = Var . Id

instance IsString Id where
  fromString = Id

instance Hashable Id where
  hash (Id i) = hash i
  hashWithSalt n (Id i) = hashWithSalt n i

unwords :: NonEmpty String -> String
unwords = concat . intersperse " "
