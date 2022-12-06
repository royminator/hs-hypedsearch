module MzML.Internal
    ( module MzML.Internal
    ) where

import Text.XML.Light
import Domain
import Prelude hiding (lookup)
import Data.Maybe (mapMaybe)
import Data.Map hiding (mapMaybe, foldl, filter, map)
import Control.Error.Util (hush)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Base64.Lazy as B64
import GHC.Float (float2Double)

data BinDataType = Float | Double
data BinaryData = BinaryData BL.ByteString BinDataType

propAccessions :: [String]
propAccessions = ["MS:1000515", "MS:1000514"]

typeAccessions :: [String]
typeAccessions = ["MS:1000521", "MS:1000523"]

qNameEq :: String -> QName -> Bool
qNameEq n (QName qn _ _) = n == qn

parseSpectrumList :: Element -> [Spectrum]
parseSpectrumList e =
    mapMaybe parseSpectrum (filterElementsName (qNameEq "spectrum") e)

parseSpectrum :: Element -> Maybe Spectrum
parseSpectrum e =
    let binArrListElm = filterElementName (qNameEq "binaryDataArrayList") e
        bin = parseBinaryArrayList binArrListElm
    in Spectrum
        <$> getSpectrumId e
        <*> (lookup "m/z array" bin >>= decodeBinaryData)
        <*> (lookup "intensity array" bin >>= decodeBinaryData)

parseBinaryArrayList :: Maybe Element -> Map String BinaryData
parseBinaryArrayList (Just e) = foldl parseBinaryData empty (elChildren e)
parseBinaryArrayList _ = empty

parseBinaryData :: Map String BinaryData -> Element -> Map String BinaryData
parseBinaryData orig e =
    let cvParams = filterElementsName (qNameEq "cvParam") e
        bin = BinaryData <$> getBinaryData e <*> getBinDataType cvParams
    in addBin (getPropertyName cvParams) bin orig
    where
        addBin (Just name) (Just bin) m = insert name bin m
        addBin _ _ m = m

getSpectrumId :: Element -> Maybe String
getSpectrumId = findAttr (unqual "id")

getPropertyName :: [Element] -> Maybe String
getPropertyName es = getAccessionName es propAccessions

getBinDataType :: [Element] -> Maybe BinDataType
getBinDataType es =
    getAccessionName es typeAccessions >>= stringToBinaryType

stringToBinaryType :: String -> Maybe BinDataType
stringToBinaryType "64-bit float" = pure Double
stringToBinaryType "32-bit float" = pure Float
stringToBinaryType _ = Nothing

getAccessionName :: [Element] -> [String] -> Maybe String
getAccessionName [] _ = Nothing
getAccessionName (e:es) acc =
    let prop = findAttr (unqual "accession") e >>= getNameIfHasAccession e acc
    in case prop of
        Just _ -> prop
        _ -> getAccessionName es acc

getNameIfHasAccession :: Element -> [String] -> String -> Maybe String
getNameIfHasAccession e accs acc =
    if acc `elem` accs
    then findAttr (unqual "name") e
    else Nothing

getBinaryData :: Element -> Maybe BL.ByteString
getBinaryData e =
    filterChildName (qNameEq "binary") e
    >>= getElementText
    >>= binStringToByteString

getElementText :: Element -> Maybe String
getElementText e =
    case (head . elContent) e of
        Text content -> pure $ cdData content
        _ -> Nothing

binStringToByteString :: String -> Maybe BL.ByteString
binStringToByteString = hush . B64.decode . BL.pack

decodeBinaryData :: BinaryData -> Maybe [Double]
decodeBinaryData (BinaryData bytes Float) = pure $ runGetFloat bytes
decodeBinaryData (BinaryData bytes Double) = pure $ runGetDouble bytes

runGetFloat :: BL.ByteString -> [Double]
runGetFloat = map float2Double . Bin.runGet (decodeBytesToFloating Bin.getFloatle)

runGetDouble :: BL.ByteString -> [Double]
runGetDouble = Bin.runGet (decodeBytesToFloating Bin.getDoublele)

decodeBytesToFloating :: Floating a => Bin.Get a -> Bin.Get [a]
decodeBytesToFloating decoder = do
    emp <- Bin.isEmpty
    if emp
    then return []
    else do val <- decoder
            vals <- decodeBytesToFloating decoder
            return (val:vals)

