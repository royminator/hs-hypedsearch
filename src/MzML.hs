module MzML
    ( readMzML
    ) where

import Text.XML.Light
import Domain
import Data.Maybe (fromMaybe)

MZ_ACCESSION :: String
MZ_ACCESSION = "MS:1000514"

INTENSITY_ACCESSION :: String
INTENSITY_ACCESSION = "MS:1000515"

readMzML :: FilePath -> IO [Spectrum]
readMzML path = do
    contents <- readFile path
    let spectrumList =
            parseXMLDoc contents >>=
            filterElementName (qNameEq "spectrumList") >>=
            parseSpectrumList

    pure $ fromMaybe [] spectrumList

qNameEq :: String -> QName -> Bool
qNameEq n (QName qn _ _) = n == qn

parseSpectrumList :: Element -> Maybe [Spectrum]
parseSpectrumList e =
    pure $ map parseSpectrum (filterElementsName (qNameEq "spectrum") e)

parseSpectrum :: Element -> Spectrum
parseSpectrum e =
    let binaryList = filterElementName (qNameEq "binaryDataArrayList") e
        mz = findBinaryWithAccession binaryList
        intensity = findBinaryWithAccession INTENSITY binaryList
    in Spectrum mz intensity

findBinaryWithAccession :: String -> Element -> [Double]
findBinaryWithAccession acc e = 

