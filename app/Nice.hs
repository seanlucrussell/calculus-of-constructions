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

infixr 7 -->

(-->) :: NiceTerm -> NiceTerm -> NiceTerm
(-->) = Implies

infixl 8 ...

(...) :: NiceTerm -> NiceTerm -> NiceTerm
(...) = App

pretty :: Term -> String
pretty (Lambda a b) = "\\" ++ pretty a ++ "." ++ pretty b
pretty (RefFree n) = "free" ++ show n
pretty (RefBound n) = show n
pretty (Pi a b) = "(forall " ++ pretty a ++ "." ++ pretty b ++ ")"
pretty (Apply a b) = "(" ++ pretty a ++ " " ++ pretty b ++ ")"
pretty Prop = "*"
pretty Type = "?"

prettyNice :: NiceTerm -> String
prettyNice (Fn n a b) = "(\\(" ++ n ++ ":" ++ prettyNice a ++ ")." ++ prettyNice b ++ ")"
prettyNice (Reference s) = s
prettyNice (ForAll n a b) = "(forall " ++ n ++ ":" ++ prettyNice a ++ "." ++ prettyNice b ++ ")"
prettyNice (Implies a b) = "(" ++ prettyNice a ++ "->" ++ prettyNice b ++ ")"
prettyNice (App a b) = "(" ++ prettyNice a ++ " " ++ prettyNice b ++ ")"
prettyNice Star = "*"

out :: NiceTerm -> IO ()
out = putStrLn . prettyNice

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

uncompileWith :: [String] -> Term -> NiceTerm
uncompileWith ctx term = case term of
  Apply te te' -> App (uncompileWith ctx te) (uncompileWith ctx te')
  Lambda te te' -> Fn ("v" ++ show (length ctx + 1)) (uncompileWith ctx te) (uncompileWith (("v" ++ show (length ctx + 1)) : ctx) te')
  Pi te te' -> ForAll ("v" ++ show (length ctx + 1)) (uncompileWith ctx te) (uncompileWith (("v" ++ show (length ctx + 1)) : ctx) te')
  RefFree n -> error "Unexpected free reference"
  RefBound n -> Reference (ctx !! n)
  Prop -> Star
  Type -> error "Not supposed to have type in expression"

typeNice :: NiceTerm -> Either String Term
typeNice term = case compile term of
  Left (context, term) -> Left ("Compilation failed:   context = " ++ show context ++ "   term = " ++ show term)
  Right te -> case typeOf te of
    Left (context, term) -> Left ("Type checking failed: context = " ++ show context ++ "   term = " ++ show term)
    Right te' -> Right te'

normNice :: NiceTerm -> Either ([Maybe String], NiceTerm) Term
normNice = fmap norm . compile

instance IsString NiceTerm where
  fromString = Reference
