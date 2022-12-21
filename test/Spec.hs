module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Hspec
import MzMLSpec
import FilteringSpec

main :: IO ()
main = do
    specs <- testSpecs filteringSpecs

    defaultMain ( testGroup "All tests"
                    [ testGroup "MzML Properties" mzMLProps
                    , testGroup "Filtering Specs" specs
                    ])

