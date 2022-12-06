module MzMLSpec
    ( mzMLSpec
    ) where

import MzML
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as B64
import Text.XML.Light
import Domain
import Lens.Micro
import Data.Binary
import Data.Binary.Put
import GHC.Float (double2Float)
import Data.AEq

data EncodingType = Float | Double

instance Arbitrary Spectrum where
    arbitrary = do
        ident <- listOf1 $ elements ['a'..'z']
        mz <- listOf1 arbitrary
        intensity <- listOf1 arbitrary
        pure $ Spectrum ident mz intensity

mzMLSpec :: Spec
mzMLSpec =
    describe "parseMzML" $ do
        it "returns the parsed version of the spectrum XML" $ property
            prop_parseMzML_parsedEqualsDeserializedInput

prop_parseMzML_parsedEqualsDeserializedInput :: Spectrum -> Bool
prop_parseMzML_parsedEqualsDeserializedInput input =
    cmpSpec ((head . parseMzML . serializeSpectrum) input) input

serializeSpectrum :: Spectrum -> BL.ByteString
serializeSpectrum = BL.pack . ppElement . spectrum2Xml

spectrum2Xml :: Spectrum -> Element
spectrum2Xml spectrum =
    let eBinaryArrayList = newElem "binaryDataArrayList" []
            (map Elem [mzBinaryArray spectrum, intBinaryArray spectrum])
        eSpectrum = newElem "spectrum" [attr "id" (spectrum ^. spId)] [Elem eBinaryArrayList]
        eSpectrumList = newElem "spectrumList" [] [Elem eSpectrum]
        eRun = newElem "run" [] [Elem eSpectrumList]
    in newElem "mzML" [] [Elem eRun]

mzBinaryArray :: Spectrum -> Element
mzBinaryArray spectrum =
    let eBinaryMz = newElem "binary" [] [createBinaryContent (spectrum ^. spMz) Double]
        cvParamMzNameAttrs = [attr "accession" "MS:1000514", attr "name" "m/z array"]
        cvParamMzTypeAttrs = [attr "accession" "MS:1000523", attr "name" "64-bit float"]
        cvParamsMz = map Elem [ newElem "cvParam" cvParamMzNameAttrs []
                              , newElem "cvParam" cvParamMzTypeAttrs []
                              ]
    in newElem "binaryDataArray" [] (cvParamsMz ++ [Elem eBinaryMz])

intBinaryArray :: Spectrum -> Element
intBinaryArray spectrum =
    let eBinaryArray = mzBinaryArray
        eBinaryInt = newElem "binary" [] [createBinaryContent (spectrum ^. spIntensity) Double]
        cvParamIntNameAttrs = [attr "accession" "MS:1000515", attr "name" "intensity array"]
        cvParamIntTypeAttrs = [attr "accession" "MS:1000521", attr "name" "32-bit float"]
        cvParamsInt = map Elem [ newElem "cvParam" cvParamIntNameAttrs []
                               , newElem "cvParam" cvParamIntTypeAttrs []
                               ]
    in newElem "binaryDataArray" [] (cvParamsInt ++ [Elem eBinaryInt])

attr :: String -> String -> Attr
attr name value = Attr (unqual name) value

newElem name attribs children = Element (unqual name) attribs children Nothing

createBinaryContent :: [Double] -> EncodingType -> Content
createBinaryContent xs t = Text $ CData CDataRaw (BL.unpack (encodeAsBinary xs t)) Nothing

encodeAsBinary :: [Double] -> EncodingType -> BL.ByteString
encodeAsBinary xs t = B64.encode (runPut (putValues xs t))

putValues :: [Double] -> EncodingType -> Put
putValues xs Float = mapM_ (putFloatle . double2Float) xs
putValues xs Double = mapM_ putDoublele xs

cmpSpec :: Spectrum -> Spectrum -> Bool
cmpSpec lhs rhs =
    let mzLen = length (lhs ^. spMz) == length (rhs ^. spMz)
        mzInt = length (lhs ^. spIntensity) == length (rhs ^. spIntensity)
        eqLen = mzLen && mzInt
        eqId = (lhs ^. spId) == (rhs ^. spId)
    in if eqLen && eqId
        then let eqMz = cmpDoubles (lhs ^. spMz) (rhs ^. spMz)
                 eqInt = cmpDoubles (lhs ^. spIntensity) (rhs ^. spIntensity)
              in eqMz && eqInt
        else False

cmpDoubles :: [Double] -> [Double] -> Bool
cmpDoubles [] [] = True
cmpDoubles (l:ls) (r:rs) =
    if cmpDouble l r
    then cmpDoubles ls rs
    else False

cmpDouble :: Double -> Double -> Bool
cmpDouble lhs rhs = lhs ~== rhs

