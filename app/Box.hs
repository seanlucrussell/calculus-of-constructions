{-# LANGUAGE OverloadedStrings #-}

module Box where

import Boolean
import Coc
import Nat
import Nice

-- maybe has type Fn "A" Star (ForAll "B" Star (("A" --> "B") --> "B" --> "B"))
-- i think. the basic thought process for building these sorts of datatypes is to determine the so-called eliminators.
-- how are you going to destroy the type? or really how are you going to turn it into something else? think of the function
-- that would let you convert a maybe value into any other sort of value. then that target value gets universally quantified,
-- the "wrapped" value (if any, note there is none for nats or bools) gets a lambda, and the body is just a complicated function
-- signature

box :: NiceTerm
box =
  Fn
    "A"
    Star
    ( Fn
        "n"
        nat
        ( ForAll
            "X"
            (nat --> Star)
            ( ForAll
                "m"
                ("X" ... zero)
                ( ForAll
                    "c"
                    (ForAll "q" nat ("A" --> ("X" ... "q") --> ("X" ... (suc ... "q"))))
                    ("X" ... "n")
                )
            )
        )
    )

emptyBoxConstructorType :: NiceTerm
emptyBoxConstructorType = ForAll "A" Star (box ... "A" ... zero)

emptyBox :: NiceTerm
emptyBox =
  Fn
    "A"
    Star
    ( Fn
        "X"
        (nat --> Star)
        ( Fn
            "cN"
            ("X" ... zero)
            ( Fn
                "cC"
                (ForAll "q" nat ("A" --> ("X" ... "q") --> ("X" ... (suc ... "q"))))
                "cN"
            )
        )
    )

buildBoxType :: NiceTerm
buildBoxType = ForAll "A" Star (ForAll "n" nat ("A" --> box ... "A" ... "n" --> box ... "A" ... (suc ... "n")))

buildBox :: NiceTerm
buildBox =
  Fn
    "A"
    Star
    ( Fn
        "n"
        nat
        ( Fn
            "x"
            "A"
            ( Fn
                "xs"
                (box ... "A" ... "n")
                ( Fn
                    "X"
                    (nat --> Star)
                    ( Fn
                        "cN"
                        ("X" ... zero)
                        ( Fn
                            "cC"
                            (ForAll "q" nat ("A" --> ("X" ... "q") --> ("X" ... (suc ... "q"))))
                            ("cC" ... "n" ... "x" ... ("xs" ... "X" ... "cN" ... "cC"))
                        )
                    )
                )
            )
        )
    )

exampleBox :: NiceTerm
exampleBox = buildBox ... boolean ... (suc ... zero) ... true ... (buildBox ... boolean ... zero ... false ... (emptyBox ... boolean))