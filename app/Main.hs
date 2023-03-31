{-# LANGUAGE OverloadedStrings #-}

module Main where

import Boolean
import Box
import Coc
import Data.Either (fromRight, isLeft, isRight)
import Equality
import List
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
    suc `hasType` (nat --> nat),
    add `hasType` (nat --> nat --> nat),
    isLeft (typeOf bad),
    true `hasType` boolean,
    false `hasType` boolean,
    commuteUnion `hasType` unionIsCommutative,
    second `hasType` secondType,
    first `hasType` firstType,
    distributeProductOverUnion `hasType` productDistributesOverUnion,
    (emptyBox ... boolean) `hasType` (box ... boolean ... zero),
    emptyBox `hasType` emptyBoxConstructorType,
    (nil ... boolean) `hasType` (list ... boolean),
    len `hasType` lenType,
    buildBox `hasType` buildBoxType,
    exampleBox `hasType` (box ... boolean ... (suc ... (suc ... zero))) -- (389.45 secs, 275,075,217,256 bytes)
  ]

main :: IO ()
main = print tests
