{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Coc where

import Control.Monad (guard, (>=>))
import Data.Either (isRight)
import Data.Foldable (find, fold)
import Data.Functor.Foldable (Corecursive (embed), cata)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust)
import Data.Set (Set, singleton)
import Debug.Trace

data Term
  = Apply Term Term
  | Lambda Term Term
  | Pi Term Term
  | RefFree Int
  | RefBound Int
  | Prop
  | Type
  deriving (Show, Eq)

makeBaseFunctor ''Term

open :: Term -> Term -> Term
open fresh = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go (RefBoundF i) depth
      | i == depth = fresh
      | otherwise = RefBound i
    go (LambdaF argType body) depth = Lambda (argType depth) (body (depth + 1))
    go (PiF argType body) depth = Pi (argType depth) (body (depth + 1))
    go term depth = embed (fmap ($ depth) term)

close :: Int -> Term -> Term
close name = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go (RefFreeF i) depth
      | i == name = RefBound depth
      | otherwise = RefFree i
    go (LambdaF argType body) depth = Lambda (argType depth) (body (depth + 1))
    go (PiF argType body) depth = Pi (argType depth) (body (depth + 1))
    go term depth = embed (fmap ($ depth) term)

sub :: Int -> Term -> Term -> Term
sub name replacement = open replacement . close name

freeVars :: Term -> Set Int
freeVars = cata (\term -> case term of RefFreeF i -> singleton i; _ -> fold term)

freshVar :: Term -> Int
freshVar term = head (filter (`notElem` freeVars term) [0 ..])

norm :: Term -> Term
norm (Apply function arg) = case norm function of
  Lambda _ body -> norm (open (norm arg) body)
  _ -> Apply (norm function) (norm arg)
norm (Pi argType body) =
  let f = freshVar body
   in Pi (norm argType) (close f (norm (open (RefFree f) body)))
norm (Lambda argType body) =
  let f = freshVar body
   in Lambda (norm argType) (close f (norm (open (RefFree f) body)))
norm term = term

typeWith :: [Term] -> Term -> Either ([Term], Term) Term
typeWith ctx term =
  let error = Left (ctx, term)
      assertSort s val = if s == Prop || s == Type then Right val else error
   in case term of
        Prop -> case ctx of
          [] -> Right Type
          t : ts -> do
            contextType <- typeWith ts t
            assertSort contextType Type
        RefBound n -> do
          contextType <- typeWith ctx Prop
          if n <= length ctx && contextType == Type
            then Right (ctx !! n)
            else error
        RefFree n -> do
          contextType <- typeWith ctx Prop
          if n <= length ctx && contextType == Type
            then Right (reverse ctx !! n)
            else error
        Pi a b -> do
          s <- typeWith (a : ctx) (open (RefFree (length ctx)) b)
          assertSort s s
        Lambda a m -> do
          b <- typeWith (a : ctx) (open (RefFree (length ctx)) m)
          s <- typeWith (a : ctx) b
          assertSort s (Pi a (close (length ctx) b))
        Apply m n -> do
          m' <- typeWith ctx m
          a <- typeWith ctx n
          case norm m' of
            Pi a' b ->
              if a' == a
                then Right (sub (length ctx - 1) n b)
                else error
            _ -> error
        Type -> error

safeIndex :: [a] -> Int -> Maybe a
safeIndex (x : xs) 0 = Just x
safeIndex (_ : xs) n = safeIndex xs (n -1)
safeIndex _ _ = Nothing

typeWith' :: [Term] -> Term -> Maybe Term
typeWith' [] Prop = Just Type
typeWith' (t : ts) Prop = do
  contextType <- typeWith' ts t
  guard (contextType == Prop || contextType == Type)
  Just Type
typeWith' ctx (RefBound n) = do
  contextType <- typeWith' ctx Prop
  guard (contextType == Type)
  safeIndex ctx n
typeWith' ctx (RefFree n) = do
  contextType <- typeWith' ctx Prop
  guard (contextType == Type)
  safeIndex (reverse ctx) n
typeWith' ctx (Pi a b) = do
  s <- typeWith' (a : ctx) (open (RefFree (length ctx)) b)
  guard (s == Prop || s == Type)
  Just s
typeWith' ctx (Lambda a m) = do
  b <- typeWith' (a : ctx) (open (RefFree (length ctx)) m)
  s <- typeWith' (a : ctx) b
  guard (s == Prop || s == Type)
  Just (Pi a (close (length ctx) b))
typeWith' ctx (Apply m n) = do
  Pi a' b <- fmap norm (typeWith' ctx m)
  a <- typeWith' ctx n
  guard (a' == a)
  Just (sub (length ctx - 1) n b)
typeWith' ctx Type = Nothing

typeOf :: Term -> Either ([Term], Term) Term
typeOf = typeWith []
