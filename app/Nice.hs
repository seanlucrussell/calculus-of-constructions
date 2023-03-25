{-# LANGUAGE OverloadedStrings #-}

module Nice where

import Coc
import Data.List (elemIndex)
import Data.String (IsString (..))

data NiceTerm
  = Fn String NiceTerm NiceTerm
  | Reference String
  | ForAll String NiceTerm NiceTerm
  | Implies NiceTerm NiceTerm
  | App NiceTerm NiceTerm
  | Star
  deriving (Show)

pretty :: Term -> String
pretty (Lambda a b) = "\\" ++ pretty a ++ "." ++ pretty b
pretty (RefFree n) = "free" ++ show n
pretty (RefBound n) = show n
pretty (Pi a b) = "(forall " ++ pretty a ++ "." ++ pretty b ++ ")"
pretty (Apply a b) = "(" ++ pretty a ++ " " ++ pretty b ++ ")"
pretty Prop = "*"
pretty Type = "?"

prettyNice :: NiceTerm -> String
prettyNice (Fn n a b) = "\\(" ++ n ++ ":" ++ prettyNice a ++ ")." ++ prettyNice b
prettyNice (Reference s) = s
prettyNice (ForAll n a b) = "forall " ++ n ++ ":" ++ prettyNice a ++ "." ++ prettyNice b
prettyNice (Implies a b) = "(" ++ prettyNice a ++ "->" ++ prettyNice b ++ ")"
prettyNice (App a b) = "(" ++ prettyNice a ++ " " ++ prettyNice b ++ ")"
prettyNice Star = "*"

compileWith :: [Maybe String] -> NiceTerm -> Either ([Maybe String], NiceTerm) Term
compileWith ctx term = case term of
  Reference s -> case elemIndex (Just s) ctx of
    Nothing -> Left (ctx, term)
    Just n -> Right (RefBound n)
  Fn s a b ->
    do
      a' <- compileWith ctx a
      b' <- compileWith (Just s : ctx) b
      return (Lambda a' b')
  ForAll s a b ->
    do
      a' <- compileWith ctx a
      b' <- compileWith (Just s : ctx) b
      return (Pi a' b')
  Implies a b ->
    do
      a' <- compileWith ctx a
      b' <- compileWith (Nothing : ctx) b
      return (Pi a' b')
  App a b ->
    do
      a' <- compileWith ctx a
      b' <- compileWith ctx b
      return (Apply a' b')
  Star -> Right Prop

compile :: NiceTerm -> Either ([Maybe String], NiceTerm) Term
compile = compileWith []

instance IsString NiceTerm where
  fromString = Reference