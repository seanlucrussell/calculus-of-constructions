{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Coc where

import Control.Monad (guard, (>=>))
import Data.Either (isRight)
import Data.Foldable (find)
import Data.Functor.Foldable (Corecursive (embed), cata)
import Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import Data.Maybe (isJust)

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

openReference :: Int -> Term -> Term
openReference fresh = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go term n = case term of
      LambdaF f g -> Lambda (f n) (g (n + 1))
      PiF f g -> Pi (f n) (g (n + 1))
      RefBoundF i
        | i == n -> RefFree fresh
        | otherwise -> RefBound i
      _ -> embed (fmap ($ n) term)

closeReference :: Int -> Term -> Term
closeReference name = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go term n = case term of
      LambdaF f g -> Lambda (f n) (g (n + 1))
      PiF f g -> Pi (f n) (g (n + 1))
      RefFreeF i
        | i == name -> RefBound n
        | otherwise -> RefFree i
      _ -> embed (fmap ($ n) term)

substitute :: Term -> Term -> Term
substitute replacement = flip (cata go) 0
  where
    go :: TermF (Int -> Term) -> Int -> Term
    go term d = case term of
      RefBoundF m
        | d == m -> replacement
        | d < m -> RefBound (m -1)
        | d > m -> RefBound m
      LambdaF f g -> Lambda (f d) (g (d + 1))
      PiF f g -> Pi (f d) (g (d + 1))
      _ -> embed (fmap ($ d) term)

type TypeError = ([Term], Term)

typeWith :: [Term] -> Term -> Either TypeError Term
typeWith ctx term =
  let error = Left (ctx, term)
   in case term of
        Prop -> Right Type
        RefBound n -> do
          contextType <- typeWith ctx Prop
          if n <= length ctx && contextType == Type
            then Right (ctx !! n)
            else error
        RefFree n -> do
          contextType <- typeWith ctx Prop
          if n <= length ctx && contextType == Type
            then Right (ctx !! (length ctx - n - 1))
            else error
        Pi a b -> do
          s <- typeWith (a : ctx) (openReference (length ctx) b)
          if s == Prop || s == Type
            then Right s
            else error
        Lambda a m -> do
          b <- typeWith (a : ctx) (openReference (length ctx) m)
          s <- typeWith (a : ctx) b
          if s == Prop || s == Type
            then Right (Pi a (closeReference (length ctx) b))
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