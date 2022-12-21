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
    { _cfgInput :: ConfigInput
    , _cfgSpectraFiltering :: ConfigSpectraFiltering
    } deriving (Show, Generic)

data ConfigInput = ConfigInput
    { _ciProteinFile :: FilePath
    , _ciSpectrumFile :: FilePath
    } deriving (Show, Generic)

data ConfigSpectraFiltering = ConfigSpectraFiltering
    { _cfNumPeaks :: Int
    , _cfRelativeAbundance :: Float
    } deriving (Show, Generic)

instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "input" <*> o .: "spectra_filtering"
    parseJSON _ = empty

instance FromJSON ConfigInput where
    parseJSON (Object o) = do
        _ciProteinFile <- o .: "protein_file"
        _ciSpectrumFile <- o .: "spectrum_file"
        return ConfigInput {..}
    parseJSON _ = empty

instance FromJSON ConfigSpectraFiltering where
    parseJSON (Object o) = do
        _cfNumPeaks <- o .: "num_peaks"
        _cfRelativeAbundance <- o .: "relative_abundance"
        return ConfigSpectraFiltering {..}
    parseJSON _ = empty

makeLenses ''Config
makeLenses ''ConfigInput
makeLenses ''ConfigSpectraFiltering
