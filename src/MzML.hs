module MzML
    ( readMzML
    ) where

import Text.XML.Light
import Domain
import Data.Maybe (mapMaybe)

data ArrayType = FLOAT_64 | FLOAT_32 deriving Eq

data BinaryData = BinaryData
    { bdProperty :: String
    , bdData :: String
    , bdType :: ArrayType
    }

prop_accessions :: [String]
prop_accessions = ["MS:1000515", "MS:1000514"]

type_accessions :: [String]
type_accessions = ["MS:1000521", "MS:1000523"]

readMzML :: FilePath -> IO [Spectrum]
readMzML path = do
    contents <- readFile path
    let spectrumList =
            parseXMLDoc contents
            >>= filterElementName (qNameEq "spectrumList")

    case spectrumList of
        Nothing -> pure []
        Just s -> pure $ parseSpectrumList s

qNameEq :: String -> QName -> Bool
qNameEq n (QName qn _ _) = n == qn

parseSpectrumList :: Element -> [Spectrum]
parseSpectrumList e =
    mapMaybe parseSpectrum (filterElementsName (qNameEq "spectrum") e)

parseSpectrum :: Element -> Maybe Spectrum
parseSpectrum e =
    filterElementName (qNameEq "binaryDataArrayList") e
    >>= parseBinaryArrayList

parseBinaryArrayList :: Element -> Maybe Spectrum
parseBinaryArrayList e =
    let binaryDatas = map parseBinaryData (elChildren e)
    in binaryDataToSpectrum binaryDatas

binaryDataToSpectrum :: [Maybe BinaryData] -> Maybe Spectrum
binaryDataToSpectrum l =
    Spectrum <$> getMz l <*> getIntensity l
    where
        getMz bs = getBinaryProp bs "m/z array"
        getIntensity bs = getBinaryProp bs "intensity array"

getBinaryProp :: [Maybe BinaryData] -> String -> Maybe String 
getBinaryProp bs prop =
    let dat = filter (\b -> (bdProperty <$> b) == Just prop) bs
    in case dat of
        [] -> Nothing
        [Just bd] -> pure $ bdData bd

parseBinaryData :: Element ->  Maybe BinaryData
parseBinaryData e =
    let cvParams = filterElementsName (qNameEq "cvParam") e
        prop = getPropertyName cvParams
        arrType = getArrayType cvParams
        dat = getBinaryData e
    in BinaryData <$> prop <*> dat <*> arrType

getPropertyName :: [Element] -> Maybe String
getPropertyName [] = Nothing
getPropertyName (e:es) =
    let prop  = findAttr (unqual "accession") e
            >>= getNameIfHasAccession e prop_accessions
    in case prop of
        Just _ -> prop
        _ -> getPropertyName es

getArrayType :: [Element] -> Maybe ArrayType
getArrayType es = Just FLOAT_32

getNameIfHasAccession :: Element -> [String] -> String -> Maybe String
getNameIfHasAccession e accs acc =
    if acc `elem` accs
    then findAttr (unqual "name") e
    else Nothing
    
getBinaryData :: Element -> Maybe String
getBinaryData e =
    filterChildName (qNameEq "binary") e
    >>= getElementText

getElementText :: Element -> Maybe String
getElementText e =
    let [Text content] = elContent e
    in pure $ cdData content
