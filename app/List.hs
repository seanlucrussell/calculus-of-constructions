{-# LANGUAGE OverloadedStrings #-}

module List where

import Boolean
import Coc
import Nat
import Nice
import Prelude hiding (succ)

list :: NiceTerm
list = Fn "A" Star (ForAll "B" Star (("A" --> "B" --> "B") --> "B" --> "B"))

nil :: NiceTerm
nil = Fn "A" Star (Fn "B" Star (Fn "c" ("A" --> "B" --> "B") (Fn "n" "B" "n")))

cons :: NiceTerm
cons = Fn "A" Star (Fn "x" "A" (Fn "xs" (list ... "A") (Fn "B" Star (Fn "c" ("A" --> "B" --> "B") (Fn "n" "B" ("c" ... "x" ... ("xs" ... "B" ... "c" ... "n")))))))

lenType :: NiceTerm
lenType = ForAll "A" Star (ForAll "l" (list ... "A") nat)

len :: NiceTerm
len = Fn "A" Star (Fn "l" (list ... "A") ("l" ... nat ... Fn "x" "A" suc ... zero))

exampleList :: NiceTerm
exampleList = cons ... boolean ... true ... (cons ... boolean ... true ... (cons ... boolean ... false ... (nil ... boolean)))

exampleListLen :: NiceTerm
exampleListLen = len ... boolean ... exampleList