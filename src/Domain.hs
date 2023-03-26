module Domain
    ( module Domain
    ) where

import           Data.Map

type Mass = Double
type Abundance = Double
type MassSpectrum = Map Mass Abundance

data AminoAcid = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | O | S | U | T | W | Y | V | B | Z | X | J
    deriving (Show, Read)

data Charge = Singly | Doubly deriving (Show, Eq)

data Alignment = Alignment {}

