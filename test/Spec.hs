module Main
    ( main
    ) where

import           FilteringSpec
import           MzMLSpec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs [peakFilterSpecs, relativeAbundanceFilterSpecs]
    defaultMain ( testGroup "All tests"
                    [ testGroup "MzML Properties" mzMLProps
                    , testGroup "Filtering Specs" specs
                    ])

