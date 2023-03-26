module FilteringSpec
    ( peakFilterSpecs
    , relativeAbundanceFilterSpecs
    ) where

import           Domain
import           Filtering
import           Test.Hspec
import Data.Map

peakFilterSpecs :: Spec
peakFilterSpecs =
    describe "takePeaks" $ do
        it "returns spectrum with highest intensity peaks" $
            shouldBe (takePeaks 2 dummySpec) peakFilteredSpec

relativeAbundanceFilterSpecs :: Spec
relativeAbundanceFilterSpecs =
    describe "relativeAbundanceFilter" $ do
        it "returns mass/intensity where intensity >= 0.25 * sum intensity == 49.45" $
            shouldBe (relativeAbundanceFilter 0.25 dummySpec) relAbFilteredSpec

dummySpec :: MassSpectrum
dummySpec = fromList [(3, 8.8), (8.9, 39), (5.2, 52), (4.3, 98)]

peakFilteredSpec :: MassSpectrum
peakFilteredSpec = fromList [(4.3, 98), (5.2, 52)]

relAbFilteredSpec :: MassSpectrum
relAbFilteredSpec = fromList [(5.2, 52), (4.3, 98)]

