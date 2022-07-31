{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Purescriptify.Utilities where

import RIO
import RIO.List as List

-- From https://hackage.haskell.org/package/extra
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs <- List.stripPrefix from xs = to ++ replace from to xs
replace from to (x : xs) = x : replace from to xs
replace _from _to [] = []