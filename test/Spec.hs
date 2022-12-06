module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Hspec
import MzMLSpec

main :: IO ()
main = do
    spec <- testSpec "mzMLSpec" mzMLSpec
    defaultMain spec

