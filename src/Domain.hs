{-# LANGUAGE TemplateHaskell #-}

module Domain
    ( module Domain
    ) where

import Lens.Micro.TH (makeLenses)

data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | O | S | U | T | W | Y | V | B | Z | X | J
    deriving (Show, Read)

data F = F32 Float | F64 Double deriving Eq

data Precursor = Precursor
    { _pcMass :: Double
    , _pcCharge :: Int
    } deriving (Show, Eq)

data Spectrum = Spectrum
    { _spId :: String
    , _spMz :: [F]
    , _spIntensity :: [F]
    , _spPrecursor :: Precursor
    } deriving (Show, Eq)

instance Show F where
    show (F32 f) = show f
    show (F64 f) = show f

makeLenses ''Spectrum
makeLenses ''Precursor

