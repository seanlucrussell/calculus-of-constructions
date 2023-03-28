{-# LANGUAGE OverloadedStrings #-}

module Boolean where

import Coc
import Nice

boolean :: NiceTerm
boolean = ForAll "A" Star ("A" --> "A" --> "A")

true :: NiceTerm
true = Fn "A" Star (Fn "x" "A" (Fn "y" "A" "x"))

false :: NiceTerm
false = Fn "A" Star (Fn "x" "A" (Fn "y" "A" "x"))