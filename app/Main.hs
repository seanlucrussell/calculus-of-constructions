{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coc
import Control.Monad (guard, (>=>))
import Data.Either (isRight)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Maybe (isJust)
import Data.String (IsString (..))

-- data Term
--   = Apply Term Term
--   | Lambda Term Term
--   | Pi Term Term
--   | Ref Int
--   | Type
--   deriving (Show, Eq)

-- substitute :: Term -> Term -> Term
-- substitute original replacement = go 0 original
--   where
--     go n (Ref m)
--       | n == m = replacement
--       | n < m = Ref (m -1)
--       | n > m = Ref m
--     go _ Type = Type
--     go n (Apply a b) = Apply (go n a) (go n b)
--     go n (Lambda a b) = Lambda (go n a) (go (n + 1) b)
--     go n (Pi a b) = Pi (go n a) (go (n + 1) b)

-- isContext :: [Term] -> Term -> Bool
-- isContext _ Type = True
-- isContext ctx (Pi a b) = isContext ctx a && isContext (a : ctx) b
-- isContext ctx (Ref n) = n <= length ctx && isContext ctx (ctx !! n)
-- isContext _ _ = False

-- judgeContext :: [Term] -> Term -> Bool
-- judgeContext [] Type = True
-- judgeContext (x : xs) Type =
--   if isContext xs x
--     then judgeContext xs x
--     else isRight (typeWith xs x)
-- judgeContext ctx (Pi a b) = judgeContext (a : ctx) b
-- judgeContext _ _ = False

-- type TypeError = ([Term], Term)

-- typeWith :: [Term] -> Term -> Either TypeError Term
-- typeWith ctx term =
--   let error = Left (ctx, term)
--    in case term of
--         Pi p n -> do
--           n' <- typeWith (p : ctx) n
--           if n' == Type
--             then Right Type
--             else error
--         Lambda p n -> fmap (Pi p) (typeWith (p : ctx) n)
--         Ref n ->
--           if n <= length ctx && judgeContext ctx Type
--             then Right (ctx !! n)
--             else error
--         Apply m n -> do
--           p' <- typeWith ctx n
--           m' <- typeWith ctx m
--           case eval m' of
--             Pi p q ->
--               if p == p'
--                 then Right (substitute n q)
--                 else error
--             _ -> error
--         Type -> error

-- typeOf :: Term -> Either TypeError Term
-- typeOf = typeWith []

-- eval :: Term -> Term
-- eval (Pi a b) = Pi (eval a) (eval b)
-- eval (Lambda a b) = Lambda (eval a) (eval b)
-- eval (Apply a b) = case eval a of
--   Lambda _ d -> eval (substitute d b)
--   Pi _ d -> eval (substitute d b)
--   e -> Apply e (eval b)
-- eval x = x

-- -- todo
-- --  write tests to verify we've done this right
-- --  make easier to use human representation that can be translated to our term type

-- data NiceTerm
--   = Fn String NiceTerm NiceTerm
--   | Reference String
--   | ForAll String NiceTerm NiceTerm
--   | Implies NiceTerm NiceTerm
--   | App NiceTerm NiceTerm
--   | Star
--   deriving (Show)

-- pretty :: Term -> String
-- pretty (Lambda a b) = "\\" ++ pretty a ++ "." ++ pretty b
-- pretty (Ref n) = show n
-- pretty (Pi a b) = "(forall " ++ pretty a ++ "." ++ pretty b ++ ")"
-- pretty (Apply a b) = "(" ++ pretty a ++ " " ++ pretty b ++ ")"
-- pretty Type = "*"

-- prettyNice :: NiceTerm -> String
-- prettyNice (Fn n a b) = "\\(" ++ n ++ ":" ++ prettyNice a ++ ")." ++ prettyNice b
-- prettyNice (Reference s) = s
-- prettyNice (ForAll n a b) = "forall " ++ n ++ ":" ++ prettyNice a ++ "." ++ prettyNice b
-- prettyNice (Implies a b) = "(" ++ prettyNice a ++ "->" ++ prettyNice b ++ ")"
-- prettyNice (App a b) = "(" ++ prettyNice a ++ " " ++ prettyNice b ++ ")"
-- prettyNice Star = "*"

-- compileWith :: [Maybe String] -> NiceTerm -> Maybe Term
-- compileWith ctx term = case term of
--   Reference s -> fmap Ref (elemIndex (Just s) ctx)
--   Fn s a b ->
--     do
--       a' <- compileWith ctx a
--       b' <- compileWith (Just s : ctx) b
--       return (Lambda a' b')
--   ForAll s a b ->
--     do
--       a' <- compileWith ctx a
--       b' <- compileWith (Just s : ctx) b
--       return (Pi a' b')
--   Implies a b ->
--     do
--       a' <- compileWith ctx a
--       b' <- compileWith (Nothing : ctx) b
--       return (Pi a' b')
--   App a b ->
--     do
--       a' <- compileWith ctx a
--       b' <- compileWith ctx b
--       return (Apply a' b')
--   Star -> Just Type

-- compile :: NiceTerm -> Maybe Term
-- compile = compileWith []

-- instance IsString NiceTerm where
--   fromString :: String -> NiceTerm
--   fromString = Reference

-- identity :: NiceTerm
-- identity = Fn "A" Star (Fn "x" "A" "x")

-- equality :: NiceTerm
-- equality = Fn "A" Star (Fn "x" "A" (Fn "y" "A" (ForAll "p" ("A" `Implies` Star) (App "p" "x" `Implies` App "p" "y"))))

-- equalityIsReflexive :: NiceTerm
-- equalityIsReflexive = ForAll "A" Star (ForAll "x" "A" (App (App (App equality "A") "x") "x"))

-- equalityIsReflexiveProof :: NiceTerm
-- equalityIsReflexiveProof = Fn "A" Star (Fn "x" "A" (Fn "p" ("A" `Implies` Star) (Fn "h" (App "p" "x") "h")))

main :: IO ()
main = putStrLn "Hello, Haskell!"
