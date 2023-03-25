{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coc
import Data.Either (isLeft)
import Nice

identity :: NiceTerm
identity = Fn "A" Star (Fn "x" "A" "x")

equality :: NiceTerm
equality = Fn "A" Star (Fn "x" "A" (Fn "y" "A" (ForAll "p" ("A" `Implies` Star) (App "p" "x" `Implies` App "p" "y"))))

equalityIsReflexive :: NiceTerm
equalityIsReflexive = ForAll "A" Star (ForAll "x" "A" (App (App (App equality "A") "x") "x"))

equalityIsReflexiveProof :: NiceTerm
equalityIsReflexiveProof = Fn "A" Star (Fn "x" "A" (Fn "p" ("A" `Implies` Star) (Fn "h" (App "p" "x") "h")))

equalityPreservedByFunctionApplicationTheorem :: NiceTerm
equalityPreservedByFunctionApplicationTheorem =
  ForAll
    "A"
    Star
    ( ForAll
        "B"
        Star
        ( ForAll
            "f"
            ("A" `Implies` "B")
            ( ForAll
                "x"
                "A"
                ( ForAll
                    "y"
                    "A"
                    ( (equality `App` "A" `App` "x" `App` "y")
                        `Implies` (equality `App` "B" `App` ("f" `App` "x") `App` ("f" `App` "y"))
                    )
                )
            )
        )
    )

-- this should type check
equalityPreservedByFunctionApplicationProof :: NiceTerm
equalityPreservedByFunctionApplicationProof =
  Fn
    "A"
    Star
    ( Fn
        "B"
        Star
        ( Fn
            "f"
            ("A" `Implies` "B")
            ( Fn
                "x"
                "A"
                ( Fn
                    "y"
                    "A"
                    ( Fn
                        "h"
                        (equality `App` "A" `App` "x" `App` "y")
                        ( "h"
                            `App` Fn
                              "u"
                              "A"
                              (equality `App` "B" `App` ("f" `App` "x") `App` ("f" `App` "u"))
                            `App` ( equalityIsReflexiveProof
                                      `App` "B"
                                      `App` ("f" `App` "x")
                                  )
                        )
                    )
                )
            )
        )
    )

-- a term that doesn't type check
bad :: Term
bad = Lambda Prop (Lambda (Lambda (RefBound 0) (RefBound 0)) (RefBound 1))

typeNice :: NiceTerm -> Either String Term
typeNice term = case compile term of
  Left (context, term) -> Left ("Compilation failed:   context = " ++ show context ++ "   term = " ++ show term)
  Right te -> case typeOf te of
    Left (context, term) -> Left ("Type checking failed: context = " ++ show context ++ "   term = " ++ show term)
    Right te' -> Right te'

tests :: Bool
tests =
  typeNice equality == Right (Pi Prop (Pi (RefBound 0) (Pi (RefBound 1) Prop)))
    && typeNice identity == Right (Pi Prop (Pi (RefBound 0) (RefBound 1)))
    && typeNice equalityIsReflexive == Right Prop
    && typeNice equalityIsReflexiveProof == Right (Pi Prop (Pi (RefBound 0) (Pi (Pi (RefBound 1) Prop) (Pi (Apply (RefBound 0) (RefBound 1)) (Apply (RefBound 1) (RefBound 2))))))
    && typeNice equalityPreservedByFunctionApplicationTheorem == Right Prop
    && isLeft (typeOf bad)

main :: IO ()
main = putStrLn "Hello, Haskell!"
