{-# LANGUAGE OverloadedStrings #-}

module Nat where

import Coc
import Nice
import Prelude hiding (succ)

nat :: NiceTerm
nat = ForAll "A" Star (("A" --> "A") --> "A" --> "A")

zero :: NiceTerm
zero = Fn "A" Star (Fn "s" ("A" --> "A") (Fn "z" "A" "z"))

suc :: NiceTerm
suc = Fn "n" nat (Fn "A" Star (Fn "s" ("A" --> "A") (Fn "z" "A" ("s" ... ("n" ... "A" ... "s" ... "z")))))

add :: NiceTerm
add = Fn "n" nat (Fn "m" nat ("n" ... nat ... suc ... "m"))
