{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( module Config
    ) where

import Data.Yaml
import GHC.Generics
import Control.Applicative
import Lens.Micro.TH

data Config = Config
    { _input :: Input
    , _spectraFiltering :: SpectraFiltering
    } deriving (Show, Generic)

data Input = Input
    { _proteinFile :: FilePath
    , _spectrumFile :: FilePath
    } deriving (Show, Generic)

data SpectraFiltering = SpectraFiltering
    { _numPeaks :: Int
    , _relativeAbundance :: Double
    } deriving (Show, Generic)

{-
data SearchParams = SearchParams
    { _minPeptideLen :: Int
    , _maxPeptideLen :: Int
    , _ppmTolerance :: Int
    , _precursorTolerance :: Int
    } deriving (Show, Generic)
-}

instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "input" <*> o .: "spectra_filtering"
    parseJSON _ = empty

instance FromJSON Input where
    parseJSON (Object o) = do
        _proteinFile <- o .: "protein_file"
        _spectrumFile <- o .: "spectrum_file"
        return Input {..}
    parseJSON _ = empty

instance FromJSON SpectraFiltering where
    parseJSON (Object o) = do
        _numPeaks <- o .: "num_peaks"
        _relativeAbundance <- o .: "relative_abundance"
        return SpectraFiltering {..}
    parseJSON _ = empty

{-
instance FromJSON SearchParams where
    parseJSON (Object o) = do
        _minPept
-}
makeLenses ''Config
makeLenses ''Input
makeLenses ''SpectraFiltering
-- makeLenses ''SearchParams
