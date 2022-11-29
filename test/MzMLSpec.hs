module MzMLSpec
    ( mzMLSpec
    ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as B64
import Text.XML.Light
import MzML
import Domain
import Lens.Micro
import Data.Binary
import Data.Binary.Put
import GHC.Float (double2Float)

data EncodingType = Float | Double

instance Arbitrary Spectrum where
    arbitrary = do
        ident <- arbitrary
        mz <- arbitrary
        intensity <- arbitrary
        return $ Spectrum ident mz intensity

mzMLSpec :: Spec
mzMLSpec = do
    describe "parseMzML" $ do
        it "returns the parsed version of the spectrum" $ property $
            prop_parseMzML_parsedEqualsDeserializedInput

prop_parseMzML_parsedEqualsDeserializedInput :: Spectrum -> Bool
prop_parseMzML_parsedEqualsDeserializedInput input =
    (head . parseMzML . serializeSpectrum) input == input

serializeSpectrum :: Spectrum -> BL.ByteString
serializeSpectrum spectrum =
    let eBinaryMz = Element (unqual "binary") [] [createBinaryContent (spectrum ^. spMz) Double] Nothing
        cvParamsMz = map Elem
            [ Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000514", Attr (unqual "name") "m/z array"] [] Nothing
            , Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000523", Attr (unqual "name") "64-bit float"] [] Nothing
            ]
        eBinaryArrayMz = Element (unqual "binaryDataArray") [] (cvParamsMz ++ [Elem eBinaryMz]) Nothing

        eBinaryInt = Element (unqual "binary") [] [createBinaryContent (spectrum ^. spIntensity) Float] Nothing
        cvParamsInt = map Elem
            [ Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000515", Attr (unqual "name") "intensity array"] [] Nothing
            , Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000521", Attr (unqual "name") "32-bit float"] [] Nothing
            ]
        eBinaryArrayInt = Element (unqual "binaryDataArray") [] (cvParamsInt ++ [Elem eBinaryInt]) Nothing

        eBinaryArrayList = Element (unqual "binaryDataArrayList") [] (map Elem [eBinaryArrayMz, eBinaryArrayInt]) Nothing
        eSpectrum = Element (unqual "spectrum") [] [Elem eBinaryArrayList] Nothing
        eSpectrumList = Element (unqual "spectrumList") [] [Elem eSpectrum] Nothing
        eRun = Element (unqual "run") [] [Elem eSpectrumList] Nothing
        eMzML = Element (unqual "mzML") [] [Elem eRun] Nothing
    in (BL.pack . ppElement) eMzML

createBinaryContent :: [Double] -> EncodingType -> Content
createBinaryContent xs t = Text $ CData CDataRaw (BL.unpack (encodeAsBinary xs t)) Nothing

encodeAsBinary :: [Double] -> EncodingType -> BL.ByteString
encodeAsBinary xs t = B64.encode (runPut (putValues xs t))

putValues :: [Double] -> EncodingType -> Put
putValues xs Float = mapM_ (putFloatle . double2Float) xs
putValues xs Double = mapM_ putDoublele xs


