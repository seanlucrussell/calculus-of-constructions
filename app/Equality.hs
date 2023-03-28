
{-# LANGUAGE OverloadedStrings #-}

module Equality where

import Coc
import Nice

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