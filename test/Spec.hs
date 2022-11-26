module Main
    ( main
    ) where

import Test.Hspec
import MzMLSpec

main :: IO ()
main = hspec mzMLSpec
