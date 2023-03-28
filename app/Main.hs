{-# LANGUAGE OverloadedStrings #-}

module Main where

import Boolean
import Coc
import Data.Either (fromRight, isLeft, isRight)
import Equality
import Logic
import Nat
import Nice
import Prelude hiding (product, succ)

-- a term that doesn't type check
bad :: Term
bad = Lambda Prop (Lambda (Lambda (RefBound 0) (RefBound 0)) (RefBound 1))

hasType :: NiceTerm -> NiceTerm -> Bool
hasType x t = case (compile x, compile t) of
  (Right x', Right t') -> case typeOf x' of
    Right t'' -> isRight (typeOf t') && norm t' == norm t''
    Left _ -> False
  _ -> False

tests :: [Bool]
tests =
  [ eq `hasType` propIsTerminal,
    identity `hasType` implicationIsReflexive,
    equalityIsReflexiveProof `hasType` equalityIsReflexive,
    equalityPreservedByFunctionApplicationProof `hasType` equalityPreservedByFunctionApplicationTheorem,
    hypotheticalSyllogismProof `hasType` hypotheticalSyllogismTheorem,
    zero `hasType` nat,
    succ `hasType` (nat --> nat),
    add `hasType` (nat --> nat --> nat),
    isLeft (typeOf bad),
    true `hasType` boolean,
    false `hasType` boolean,
    commuteUnion `hasType` unionIsCommutative,
    second `hasType` secondType,
    first `hasType` firstType,
    distributeProductOverUnion `hasType` productDistributesOverUnion
  ]

main :: IO ()
main = print tests
