{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AlignmentSpec
    ( identifySpectrumSpec
    ) where

import Domain
import Alignment
import Test.Hspec
import Data.Map (fromList)

identifySpectrumSpec :: Spec
identifySpectrumSpec =
    describe "identifySpectrum" $ do
        it "returns k-mers that best describe the observed mass spectrum" $
            let spectrum = fromList [(32.8, 80), (2.18, 331), (981.1, 92)]
                store = KMerStore [ KMerData{ locStart = 3, locEnd = 4, parentProtein = 3, ion = B_ION, mass = 33 }
                                  , KMerData{ locStart = 81, locEnd = 93, parentProtein = 23, ion = B_ION, mass = 8.3 }
                                  ]
                expected = [head store.kMers]
                actual = (identifySpectrum spectrum store 0.4)
            in expected `shouldBe` actual
