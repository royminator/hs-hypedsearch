{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Alignment
    ( module Alignment
    ) where

import Domain
import Data.Map (keys)

data KMerData = KMerData
    { len :: Int
    , locEnd :: Int
    , parentProtein :: Int
    , ion :: FragmentIon
    , mass :: Mass
    } deriving (Show, Eq)

data FragmentCandidates = FragmentCandidates
    { bCandidates :: [KMerData]
    , yCandidates :: [KMerData]
    } deriving (Show, Eq)

newtype KMerStore = KMerStore { kMers :: [KMerData] }

identifySpectrum :: MassSpectrum -> KMerStore -> Mass -> FragmentCandidates
identifySpectrum spec store tol =
    filter (isFragmentDescribed (keys spec) tol) store.kMers
    where
        isFragmentDescribed :: [Mass] -> Mass -> KMerData -> Bool
        isFragmentDescribed masses tolerance kmer =
            any (\m -> abs (kmer.mass - m) < tolerance) masses

