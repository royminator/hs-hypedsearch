{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Alignment
    ( module Alignment
    ) where

import Domain

data KMerStore = KMerStore
    { kMers :: [Peptide]
    }

identifySpectrum :: MassSpectrum -> KMerStore -> [PeptideCandidate]
identifySpectrum spec store = undefined
