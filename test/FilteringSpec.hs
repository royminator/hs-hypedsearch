module FilteringSpec
    ( peakFilterSpecs
    , relativeAbundanceFilterSpecs
    ) where

import Test.Hspec
import Domain
import Filtering

peakFilterSpecs :: Spec
peakFilterSpecs =
    describe "takePeaks" $ do
        it "returns spectrum with highest intensity peaks" $
            shouldBe (applyFilter (takePeaks 2) dummySpec) peakFilteredSpec

relativeAbundanceFilterSpecs :: Spec
relativeAbundanceFilterSpecs =
    describe "relativeAbundanceFilter" $ do
        it "returns mass/intensity where intensity >= 0.25 * sum intensity == 49.45" $
            shouldBe (applyFilter (relativeAbundanceFilter 0.25) dummySpec) relAbFilteredSpec

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

relAbFilteredSpec :: Spectrum
relAbFilteredSpec =
    Spectrum "dummy"
        [5.2, 4.3] [52, 98]
        (Precursor 4 8)
        
