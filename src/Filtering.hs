module Filtering
    ( module Filtering
    ) where

import Domain
import Config
import Data.List (sortBy)
import Data.Ord (comparing)
import Lens.Micro

applyFilter :: (Spectrum -> Spectrum) -> Spectrum -> Spectrum
applyFilter f = f

getSpectraFilter :: ConfigSpectraFiltering -> (Spectrum -> Spectrum)
getSpectraFilter (ConfigSpectraFiltering 0 f) = relativeAbundanceFilter f
getSpectraFilter (ConfigSpectraFiltering n _) = takePeaks n

takePeaks :: Int -> Spectrum -> Spectrum
takePeaks n s =
    let (masses, intensities) = unzip .
            sortBy (comparing fst) .
            take n . 
            sortBy (flip (comparing snd)) $
            zip (s^.spMz) (s^.spIntensity)
    in Spectrum (s^.spId) masses intensities (s^.spPrecursor)

relativeAbundanceFilter :: Floating a => a -> Spectrum -> Spectrum
relativeAbundanceFilter f s = undefined
{-
    let totalIntensity = sum s^.spIntensity
        minVal = totalIntensity * f
        (masses, intensities) = filter (\(_, i) -> i >= minVal) $ zip (s^.spMz) (s^.spIntensity)
    in Spectrum (s^.spId) masses intensities (s^.spPrecursor)
-}
