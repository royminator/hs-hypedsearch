module AlignmentSpec
    ( identifySpectrumSpec
    ) where

import Domain
import Alignment
import Test.Hspec

identifySpectrumSpec :: Spec
identifySpectrumSpec =
    describe "identifySpectrum" $ do
        it "returns k-mers that best describe the observed mass spectrum" $
            let spectrum = undefined :: MassSpectrum
                store = undefined :: KMerStore
                expected = undefined :: [PeptideCandidate]
            in (identifySpectrum spectrum store) `shouldBe` expected
