module FilteringSpec
    ( filteringSpecs
    ) where

import Test.Hspec
import Domain
import Filtering

filteringSpecs :: Spec
filteringSpecs =
    describe "takePeaks" $ do
        it "returns spectrum with highest intensity peaks" $
            shouldBe (applyFilter (takePeaks 2) dummySpec) peakFilteredSpec

dummySpec :: Spectrum
dummySpec =
    Spectrum "dummy" 
        [3, 8.9, 5.2, 4.3] [8.8, 39, 52, 98]
        (Precursor 4 8)

peakFilteredSpec :: Spectrum 
peakFilteredSpec =
    Spectrum "dummy"
        [4.3, 5.2] [98, 52]
        (Precursor 4 8)
