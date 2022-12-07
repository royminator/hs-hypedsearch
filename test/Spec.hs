module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Hedgehog
import MzMLSpec

main :: IO ()
main = do
    defaultMain ( testGroup "All tests" [
                    testGroup "Properties" mzMLProps
                ])

