{-# LANGUAGE OverloadedStrings #-}

module Logic where

import Coc
import Nice
import Prelude hiding (product, union)

implicationIsReflexive :: NiceTerm
implicationIsReflexive = ForAll "A" Star ("A" --> "A")

identity :: NiceTerm
identity = Fn "A" Star (Fn "x" "A" "x")

propIsTerminal :: NiceTerm
propIsTerminal = ForAll "A" Star ("A" --> ("A" --> Star))

hypotheticalSyllogismTheorem :: NiceTerm
hypotheticalSyllogismTheorem = ForAll "A" Star (ForAll "B" Star (ForAll "C" Star (("A" --> "B") --> ("B" --> "C") --> "A" --> "C")))

hypotheticalSyllogismProof :: NiceTerm
hypotheticalSyllogismProof = Fn "A" Star (Fn "B" Star (Fn "C" Star (Fn "f" ("A" --> "B") (Fn "g" ("B" --> "C") (Fn "x" "A" ("g" ... ("f" ... "x")))))))

product :: NiceTerm -> NiceTerm -> NiceTerm
product a b = ForAll "C" Star ((a --> b --> "C") --> "C")

pair :: NiceTerm
pair = Fn "A" Star (Fn "B" Star (Fn "x" "A" (Fn "y" "B" (Fn "C" Star (Fn "f" ("A" --> "B" --> "C") ("f" ... "x" ... "y"))))))

union :: NiceTerm -> NiceTerm -> NiceTerm
union a b = ForAll "C" Star ((a --> "C") --> (b --> "C") --> "C")

cases :: NiceTerm
cases = Fn "A" Star (Fn "B" Star (Fn "u" (union "A" "B") (Fn "C" Star (Fn "f" ("A" --> "C") (Fn "g" ("B" --> "C") ("u" ... "C" ... "f" ... "g"))))))

not :: NiceTerm -> NiceTerm
not a = ForAll "C" Star (a --> "C")

unionIsCommutative :: NiceTerm
unionIsCommutative = ForAll "A" Star (ForAll "B" Star (union "A" "B" --> union "B" "A"))

commuteUnion :: NiceTerm
commuteUnion =
  Fn
    "A"
    Star
    ( Fn
        "B"
        Star
        ( Fn
            "x"
            ("A" `union` "B")
            ( Fn
                "C"
                Star
                ( Fn
                    "f"
                    ("B" --> "C")
                    ( Fn
                        "g"
                        ("A" --> "C")
                        ("x" ... "C" ... "g" ... "f")
                    )
                )
            )
        )
    )

firstType :: NiceTerm
firstType = ForAll "A" Star (ForAll "B" Star (product "A" "B" --> "A"))

first :: NiceTerm
first = Fn "A" Star (Fn "B" Star (Fn "p" (product "A" "B") ("p" ... "A" ... Fn "x" "A" (Fn "y" "B" "x"))))

secondType :: NiceTerm
secondType = ForAll "A" Star (ForAll "B" Star (product "A" "B" --> "B"))

second :: NiceTerm
second = Fn "A" Star (Fn "B" Star (Fn "p" (product "A" "B") ("p" ... "B" ... Fn "x" "A" (Fn "y" "B" "y"))))

left :: NiceTerm
left = Fn "A" Star (Fn "B" Star (Fn "x" "A" (Fn "C" Star (Fn "f" ("A" --> "C") (Fn "g" ("B" --> "C") ("f" ... "x"))))))

right :: NiceTerm
right = Fn "A" Star (Fn "B" Star (Fn "y" "B" (Fn "C" Star (Fn "f" ("A" --> "C") (Fn "g" ("B" --> "C") ("g" ... "y"))))))

productDistributesOverUnion :: NiceTerm
productDistributesOverUnion =
  ForAll
    "P"
    Star
    ( ForAll
        "Q"
        Star
        ( ForAll
            "R"
            Star
            ( ("P" `product` ("Q" `union` "R"))
                --> (("P" `product` "Q") `union` ("P" `product` "R"))
            )
        )
    )

-- distributeProductOverUnion =
--  \(P:*)(Q:*)(R:*)(a:product P (union Q R)).
-- (second P (union Q R) a) (union (product P Q) (product P R))  (\q:Q. left (product P Q) (product P R) (pair P Q (first P (union Q R) a) q))  (\r:R. right (product P Q) (product P R) (pair P R (first P (union Q R) a) r))
distributeProductOverUnion :: NiceTerm
distributeProductOverUnion =
  Fn
    "P"
    Star
    ( Fn
        "Q"
        Star
        ( Fn
            "R"
            Star
            ( Fn
                "a"
                ("P" `product` ("Q" `union` "R"))
                ( (second ... "P" ... union "Q" "R" ... "a") ... union (product "P" "Q") (product "P" "R")
                    ... Fn
                      "q"
                      "Q"
                      (left ... product "P" "Q" ... product "P" "R" ... (pair ... "P" ... "Q" ... (first ... "P" ... union "Q" "R" ... "a") ... "q"))
                    ... Fn
                      "r"
                      "R"
                      (right ... product "P" "Q" ... product "P" "R" ... (pair ... "P" ... "R" ... (first ... "P" ... union "Q" "R" ... "a") ... "r"))
                )
            )
        )
    )