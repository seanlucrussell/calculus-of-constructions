{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Coc where

import Control.Monad (guard, (>=>))
import Data.Either (isRight)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Maybe (isJust)
import Data.String (IsString (..))

data Term
  = Apply Term Term
  | Lambda Term Term
  | Pi Term Term
  | Ref Int
  | Prop
  | Type
  deriving (Show, Eq)

instance Num Term where
  fromInteger = Ref . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

updateReferences :: (Int -> Int -> Term) -> Term -> Term
updateReferences f = go 0
  where
    go d term = case term of
      Ref n -> f d n
      Type -> Type
      Prop -> Prop
      Apply a b -> Apply (go d a) (go d b)
      Lambda a b -> Lambda (go d a) (go (d + 1) b)
      Pi a b -> Pi (go d a) (go (d + 1) b)

substitute :: Term -> Term -> Term
substitute replacement = updateReferences go
  where
    go d m
      | d == m = replacement
      | d < m = Ref (m -1)
      | d > m = Ref m

changeFreeVarDepth :: Int -> Term -> Term
changeFreeVarDepth n = updateReferences go
  where
    go d m
      | n > d = Ref (m + n)
      | n <= d = Ref m

type TypeError = ([Term], Term)

typeWith :: [Term] -> Term -> Either TypeError Term
typeWith ctx term =
  let error = Left (ctx, term)
   in case term of
        Prop -> Right Type
        Ref n -> do
          contextType <- typeWith ctx Prop
          if n <= length ctx && contextType == Type
            then Right (changeFreeVarDepth (n + 1) (ctx !! n))
            else error
        Pi a b -> do
          s <- typeWith (a : ctx) b
          if s == Prop || s == Type
            then Right s
            else error
        Lambda a m -> do
          b <- typeWith (a : ctx) m
          -- bug here
          -- if 'a' references a reference, then that second reference gets put into 'b'.
          -- but consider that, since we are using de Bruijin indexing, the reference 'b' means a different thing
          -- if it is in the first position of the context, or the third, or the eighth, or pulled out of the context entirely.
          -- So we have to account for that somehow. we need to make sure we reindex b so that it is pointing at the same thing as it was originally.
          -- but we only do this for references.
          -- i guess some people resolve this with a so-called "locally nameless" representation. the idea is you have de Bruijin indexes for
          -- bound variables in a term and you have normal names for free variables. althought tbh i'm not quite sure how this would help because i think
          -- you'd still end up with local variables in the context. perhaps you just make sure that you
          -- always convert to the named representation before adding things to the context?

          -- it may be that the easiest solution is to convert to an entirely named system
          s <- typeWith (a : ctx) b
          if s == Prop || s == Type
            then Right (Pi a b)
            else error
        Apply m n -> do
          m' <- typeWith ctx m
          a <- typeWith ctx n
          case eval m' of
            Pi a' b ->
              if a' == a
                then Right (substitute n b)
                else error
            _ -> error
        Type -> error

typeOf :: Term -> Either TypeError Term
typeOf = typeWith []

eval :: Term -> Term
eval (Pi a b) = Pi (eval a) (eval b)
eval (Lambda a b) = Lambda (eval a) (eval b)
eval (Apply a b) = case eval a of
  Lambda _ d -> eval (substitute d b)
  Pi _ d -> eval (substitute d b)
  e -> Apply e (eval b)
eval x = x

-- todo
--  write tests to verify we've done this right
--  make easier to use human representation that can be translated to our term type

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
pretty (Ref n) = show n
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

compileWith :: [Maybe String] -> NiceTerm -> Maybe Term
compileWith ctx term = case term of
  Reference s -> fmap Ref (elemIndex (Just s) ctx)
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
  Star -> Just Prop

compile :: NiceTerm -> Maybe Term
compile = compileWith []

instance IsString NiceTerm where
  fromString :: String -> NiceTerm
  fromString = Reference

identity :: NiceTerm
identity = Fn "A" Star (Fn "x" "A" "x")

equality :: NiceTerm
equality = Fn "A" Star (Fn "x" "A" (Fn "y" "A" (ForAll "p" ("A" `Implies` Star) (App "p" "x" `Implies` App "p" "y"))))

equalityIsReflexive :: NiceTerm
equalityIsReflexive = ForAll "A" Star (ForAll "x" "A" (App (App (App equality "A") "x") "x"))

equalityIsReflexiveProof :: NiceTerm
equalityIsReflexiveProof = Fn "A" Star (Fn "x" "A" (Fn "p" ("A" `Implies` Star) (Fn "h" (App "p" "x") "h")))
