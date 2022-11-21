{-# LANGUAGE TemplateHaskell #-}

module Domain
    ( AA (..)
    , Spectrum (..)
    , spMz
    , spIntensity
    ) where

import Lens.Micro.TH (makeLenses)

data AA = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | O | S | U | T | W | Y | V | B | Z | X | J
    deriving (Show, Read)


data Spectrum = Spectrum
    { _spMz :: String
    , _spIntensity :: String
    } deriving (Show)

makeLenses ''Spectrum

