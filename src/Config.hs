{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( Config
    , ConfigInput
    , cfgInput
    , ciProteinFile
    , ciSpectrumFile
    ) where

import Data.Yaml
import GHC.Generics
import Control.Applicative
import Lens.Micro.TH

data Config = Config
    { _cfgInput :: ConfigInput
    } deriving (Show, Generic)

data ConfigInput = ConfigInput
    { _ciProteinFile :: FilePath
    , _ciSpectrumFile :: FilePath
    } deriving (Show, Generic)

instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "input"
    parseJSON _ = empty

instance FromJSON ConfigInput where
    parseJSON (Object o) = do
        _ciProteinFile <- o .: "protein_file"
        _ciSpectrumFile <- o .: "spectrum_file"
        return ConfigInput { .. }
    parseJSON _ = empty

makeLenses ''Config
makeLenses ''ConfigInput
