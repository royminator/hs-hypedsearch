module Main
    ( main
    ) where

import Test.Tasty
import MzMLSpec

main :: IO ()
main = do
    defaultMain ( testGroup "All tests" [
                    testGroup "Properties" mzMLProps
                ])

