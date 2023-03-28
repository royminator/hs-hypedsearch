{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Alignment
    ( module Alignment
    ) where

import Domain
import Data.Map (keys)

data KMerData = KMerData
    { locStart :: Int
    , locEnd :: Int
    , parentProtein :: Int
    , ion :: FragmentIon
    , mass :: Mass
    } deriving (Show, Eq)

data KMerStore = KMerStore
    { kMers :: [KMerData]
    }

identifySpectrum :: MassSpectrum -> KMerStore -> Mass -> [KMerData]
identifySpectrum spec store tol =
    filter (isFragmentDescribed (keys spec) tol) store.kMers
    where
        isFragmentDescribed :: [Mass] -> Mass -> KMerData -> Bool
        isFragmentDescribed masses tol kmer =
            any (\m -> abs (kmer.mass - m) < tol) masses
