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

openReference :: Term -> Term -> Term
openReference fresh = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go term depth = case term of
      LambdaF argType body -> Lambda (argType depth) (body (depth + 1))
      PiF argType body -> Pi (argType depth) (body (depth + 1))
      RefBoundF i
        | i == depth -> fresh
        | otherwise -> RefBound i
      _ -> embed (fmap ($ depth) term)

closeReference :: Int -> Term -> Term
closeReference name = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go term depth = case term of
      LambdaF argType body -> Lambda (argType depth) (body (depth + 1))
      PiF argType body -> Pi (argType depth) (body (depth + 1))
      RefFreeF i
        | i == name -> RefBound depth
        | otherwise -> RefFree i
      _ -> embed (fmap ($ depth) term)

sub :: Int -> Term -> Term -> Term
sub name replacement = openReference replacement . closeReference name

locallyClosed :: Term -> Bool
locallyClosed = flip (cata go) 0
 where
  go :: TermF (Int -> Bool) -> Int -> Bool
  go (RefBoundF n) depth = n <= depth
  go (LambdaF argType body) depth = argType depth && body (depth + 1)
  go (PiF argType body) depth = argType depth && body (depth + 1)
  go term depth = and (fmap ($ depth) term)

freeVars :: Term -> Set Int
freeVars = cata go
  where go :: TermF (Set Int) -> Set Int
        go (RefFreeF i) = singleton i
        go term = fold term

freshVar :: Term -> Int
freshVar term = head (filter (`notElem` freeVars term) [0..])

norm :: Term -> Term
norm term = case term of
  Apply function arg -> case norm function of
    Lambda _ body -> norm (openReference (norm arg) body)
    _ -> Apply (norm function) (norm arg)
  Pi argType body -> 
    let f = freshVar body 
    in Pi (norm argType) (closeReference f (norm (openReference (RefFree f) body)))
  Lambda argType body -> 
    let f = freshVar body 
    in Lambda (norm argType) (closeReference f (norm (openReference (RefFree f) body)))
  _ -> term

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
          s <- typeWith (a : ctx) (openReference (RefFree (length ctx)) b)
          assertSort s s
        Lambda a m -> do
          b <- typeWith (a : ctx) (openReference (RefFree (length ctx)) m)
          s <- typeWith (a : ctx) b
          assertSort s (Pi a (closeReference (length ctx) b))
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

typeOf :: Term -> Either ([Term], Term) Term
typeOf = typeWith []
