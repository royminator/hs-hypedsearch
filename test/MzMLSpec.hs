module MzMLSpec
    ( mzMLProps
    ) where

import MzML.Internal
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as B64
import Text.XML.Light
import Domain
import Lens.Micro
import Data.Binary
import Data.Binary.Put

mzMLProps :: [TestTree]
mzMLProps = [ testProperty "encoded and decoded [F32] equals input" prop_encodeDecodeF32 ]

prop_encodeDecodeF32 :: Property
prop_encodeDecodeF32 = property $ do
    floats <- forAll $ Gen.list (Range.linear 0 100) genF32
    let bin = BinaryData (encodeAsBinary floats) Float
        decoded = decodeBinaryData bin
    decoded === Just floats

genF32 :: Gen F
genF32 = do
    let (min, max) = (-99 :: Float, 99 :: Float)
    f <- Gen.float $ Range.linearFrac min max
    pure $ F32 f

encodeAsBinary :: [F] -> BL.ByteString
encodeAsBinary xs = runPut $ putValues xs

putValues :: [F] -> Put
putValues = mapM_ putValue

putValue :: F -> Put
putValue (F32 f) = putFloatle f
putValue (F64 f) = putDoublele f

    {-
    describe "parseMzML" $ do
        it "returns the parsed version of the spectrum XML" $ property
            prop_parseMzML_parsedEqualsDeserializedInput

prop_parseMzML_parsedEqualsDeserializedInput :: Spectrum -> Bool
prop_parseMzML_parsedEqualsDeserializedInput input =
    (head . parseMzML . serializeSpectrum) input == input

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
    let eBinaryMz = newElem "binary" [] [createBinaryContent (spectrum ^. spMz)]
        cvParamMzNameAttrs = [attr "accession" "MS:1000514", attr "name" "m/z array"]
        cvParamMzTypeAttrs = [attr "accession" "MS:1000523", attr "name" "64-bit float"]
        cvParamsMz = map Elem [ newElem "cvParam" cvParamMzNameAttrs []
                              , newElem "cvParam" cvParamMzTypeAttrs []
                              ]
    in newElem "binaryDataArray" [] (cvParamsMz ++ [Elem eBinaryMz])

intBinaryArray :: Spectrum -> Element
intBinaryArray spectrum =
    let eBinaryArray = mzBinaryArray
        eBinaryInt = newElem "binary" [] [createBinaryContent (spectrum ^. spIntensity)]
        cvParamIntNameAttrs = [attr "accession" "MS:1000515", attr "name" "intensity array"]
        cvParamIntTypeAttrs = [attr "accession" "MS:1000521", attr "name" "32-bit float"]
        cvParamsInt = map Elem [ newElem "cvParam" cvParamIntNameAttrs []
                               , newElem "cvParam" cvParamIntTypeAttrs []
                               ]
    in newElem "binaryDataArray" [] (cvParamsInt ++ [Elem eBinaryInt])

attr :: String -> String -> Attr
attr name value = Attr (unqual name) value

newElem name attribs children = Element (unqual name) attribs children Nothing

createBinaryContent :: [F] -> Content
createBinaryContent xs = Text $ CData CDataRaw (BL.unpack (encodeAsBinary xs)) Nothing

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
-}
