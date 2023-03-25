{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coc
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

(t0, t1) = ([RefFree 0, Prop], Pi (RefFree 1) (RefFree 0))

-- this shouldn't type check, i think. but when type checking is run we get
--      Pi Prop (    Pi (Lambda (RefBound 0)                                    (RefBound 0)) Prop)
w = Lambda Prop (Lambda (Apply (Lambda Prop (Lambda (RefBound 0) (RefBound 0))) (RefBound 0)) (RefBound 1))

x =
  Lambda
    Prop
    ( Lambda
        Prop
        ( Lambda
            ( Pi
                (RefBound 1)
                (RefBound 1)
            )
            ( Lambda
                (RefBound 2)
                ( Lambda
                    (RefBound 3)
                    ( Lambda
                        ( Apply
                            ( Apply
                                (RefBound 3)
                                (RefBound 1)
                            )
                            (RefBound 0)
                        )
                        ( Apply
                            ( Apply
                                (RefBound 0)
                                ( Lambda
                                    (RefBound 5)
                                    ( Apply
                                        ( Apply
                                            (RefBound 4)
                                            ( Apply
                                                (RefBound 4)
                                                (RefBound 3)
                                            )
                                        )
                                        ( Apply
                                            (RefBound 4)
                                            (RefBound 0)
                                        )
                                    )
                                )
                            )
                            ( Apply
                                (RefBound 3)
                                ( Apply
                                    (RefBound 3)
                                    (RefBound 2)
                                )
                            )
                        )
                    )
                )
            )
        )
    )

breakCtx =
  [ Apply (Apply (Apply (Lambda Prop (Lambda (RefBound 0) (Lambda (RefBound 1) (Pi (Pi (RefBound 2) Prop) (Pi (Apply (RefBound 0) (RefBound 2)) (Apply (RefBound 1) (RefBound 2))))))) (RefFree 0)) (RefFree 3)) (RefFree 4),
    RefFree 0,
    RefFree 0,
    Pi (RefFree 0) (RefFree 1),
    Prop,
    Prop
  ]

breakTerm =
  Apply (RefFree 5) (Lambda (RefFree 0) (Apply (Apply (Apply (Lambda Prop (Lambda (RefBound 0) (Lambda (RefBound 1) (Pi (Pi (RefBound 2) Prop) (Pi (Apply (RefBound 0) (RefBound 2)) (Apply (RefBound 1) (RefBound 2))))))) (RefFree 1)) (Apply (RefFree 2) (RefFree 3))) (Apply (RefFree 2) (RefBound 0))))

typePretty :: NiceTerm -> Either String Term
typePretty term = case compile term of
  Left (context, term) -> Left ("Compilation failed:   context = " ++ show context ++ "   term = " ++ show term)
  Right te -> case typeOf te of
    Left (context, term) -> Left ("Type checking failed: context = " ++ show context ++ "   term = " ++ show term)
    Right te' -> Right te'

main :: IO ()
main = putStrLn "Hello, Haskell!"
