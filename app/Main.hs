{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coc
import Data.Either (fromRight, isLeft, isRight)
import Nice
import Prelude hiding (succ)

implicationIsReflexive :: NiceTerm
implicationIsReflexive = ForAll "A" Star ("A" --> "A")

identity :: NiceTerm
identity = Fn "A" Star (Fn "x" "A" "x")

propIsTerminal :: NiceTerm
propIsTerminal = ForAll "A" Star ("A" --> ("A" --> Star))

eq :: NiceTerm
eq = Fn "A" Star (Fn "x" "A" (Fn "y" "A" (ForAll "p" ("A" --> Star) ("p" ... "x" --> "p" ... "y"))))

equalityIsReflexive :: NiceTerm
equalityIsReflexive = ForAll "A" Star (ForAll "x" "A" (eq ... "A" ... "x" ... "x"))

equalityIsReflexiveProof :: NiceTerm
equalityIsReflexiveProof = Fn "A" Star (Fn "x" "A" (Fn "p" ("A" --> Star) (Fn "h" ("p" ... "x") "h")))

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
            ("A" --> "B")
            ( ForAll
                "x"
                "A"
                ( ForAll
                    "y"
                    "A"
                    ( (eq ... "A" ... "x" ... "y")
                        --> (eq ... "B" ... ("f" ... "x") ... ("f" ... "y"))
                    )
                )
            )
        )
    )

-- this should type check. it is written correctly.
-- we are somehow ending up with bound terms with no corresponding context. probably because i'm using the whole eval pipeline wrong.
-- eval and substitute should probably only use the locally nameless representation stuff instead of manually figuring out the indexing.
-- actually idk if this is true. hold up
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
            ("A" --> "B")
            ( Fn
                "x"
                "A"
                ( Fn
                    "y"
                    "A"
                    ( Fn
                        "h"
                        (eq ... "A" ... "x" ... "y")
                        ( "h"
                            ... Fn "u" "A" (eq ... "B" ... ("f" ... "x") ... ("f" ... "u"))
                            ... (equalityIsReflexiveProof ... "B" ... ("f" ... "x"))
                        )
                    )
                )
            )
        )
    )

ctx :: [Term]
ctx =
  [ Apply (RefFree 7) (Apply (RefFree 2) (RefFree 3)),
    Pi (RefFree 1) Prop,
    RefFree 0,
    Pi (Pi (RefFree 0) Prop) (Pi (Apply (RefBound 0) (RefFree 3)) (Apply (RefBound 1) (RefFree 4))),
    RefFree 0,
    RefFree 0,
    Pi (RefFree 0) (RefFree 1),
    Prop,
    Prop
  ]

term :: Term
term =
  Apply
    (RefFree 2)
    (RefFree 8)

nat :: NiceTerm
nat = ForAll "A" Star (("A" --> "A") --> "A" --> "A")

zero :: NiceTerm
zero = Fn "A" Star (Fn "s" ("A" --> "A") (Fn "z" "A" "z"))

succ :: NiceTerm
succ = Fn "n" nat (Fn "A" Star (Fn "s" ("A" --> "A") (Fn "z" "A" ("s" ... ("n" ... "A" ... "s" ... "z")))))

add :: NiceTerm
add = Fn "n" nat (Fn "m" nat ("n" ... nat ... succ ... "m"))

hypotheticalSyllogismTheorem :: NiceTerm
hypotheticalSyllogismTheorem = ForAll "A" Star (ForAll "B" Star (ForAll "C" Star (("A" --> "B") --> ("B" --> "C") --> "A" --> "C")))

hypotheticalSyllogismProof :: NiceTerm
hypotheticalSyllogismProof = Fn "A" Star (Fn "B" Star (Fn "C" Star (Fn "f" ("A" --> "B") (Fn "g" ("B" --> "C") (Fn "x" "A" ("g" ... ("f" ... "x")))))))

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
    isLeft (typeOf bad)
  ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
