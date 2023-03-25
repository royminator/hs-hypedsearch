{-# LANGUAGE TemplateHaskell #-}

module Domain
    ( module Domain
    ) where

import           Lens.Micro.TH (makeLenses)

data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | O | S | U | T | W | Y | V | B | Z | X | J
    deriving (Show, Read)

data Precursor = Precursor
    { _pcMass   :: Double
    , _pcCharge :: Int
    } deriving (Show, Eq)

data Spectrum = Spectrum
    { _spId        :: String
    , _spMz        :: [Double]
    , _spIntensity :: [Double]
    , _spPrecursor :: Precursor
    } deriving (Show, Eq)

makeLenses ''Spectrum
makeLenses ''Precursor

