module Filtering
    ( module Filtering
    ) where

import Domain
import Config
import Data.List (sortBy)
import Data.Ord (comparing)
import Lens.Micro
import Data.Map (keys, elems, fromList, filter)

getSpectraFilter :: SpectraFiltering -> (MassSpectrum -> MassSpectrum)
getSpectraFilter (SpectraFiltering 0 f) = relativeAbundanceFilter f
getSpectraFilter (SpectraFiltering n _) = takePeaks n

takePeaks :: Int -> MassSpectrum -> MassSpectrum
takePeaks n s =
    fromList
    . sortBy (comparing fst)
    . take n
    . sortBy (flip (comparing snd)) $
        zip (keys s) (elems s)


relativeAbundanceFilter :: Double -> MassSpectrum -> MassSpectrum
relativeAbundanceFilter d s =
    let minVal = d * sum (elems s)
    in Data.Map.filter (>= minVal) s

