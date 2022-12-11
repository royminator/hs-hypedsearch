module MzMLSpec
    ( mzMLProps
    ) where

import MzML.Internal
import qualified MzML.Parser as MzML
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as B64
import Domain
import Lens.Micro
import Data.Binary
import Data.Binary.Put
import Text.Printf
import Hedgehog.Internal.Range (linearFrac)
import Domain (Precursor(Precursor))

mzMLProps :: [TestTree]
mzMLProps = [ testProperty "encoded and decoded [F32] equals input" prop_encodeDecodeF32
            , testProperty "verify parsed then serialized spectrum equals input" prop_serializeParseMzML 
            ]

prop_encodeDecodeF32 :: Property
prop_encodeDecodeF32 = property $ do
    floats <- forAll $ Gen.list (Range.linear 0 100) genF32
    let bin = BinaryData (encodeAsBinary floats) Float
    decodeBinaryData bin === Just floats

prop_serializeParseMzML :: Property
prop_serializeParseMzML = property $ do
    spectrum <- forAll genSpectrum
    let serialized = serializeSpectrum spectrum
        parsed = head $ MzML.parseMzML serialized
    parsed === spectrum

genSpectrum :: Gen Spectrum
genSpectrum = do
    ident <- Gen.string (Range.linear 1 100) Gen.alphaNum
    mz <- Gen.list (Range.linear 1 100) genF64
    intensity <- Gen.list (Range.linear 1 100) genF32
    precursor <- genPrecursor
    pure $ Spectrum ident mz intensity precursor

genF32 :: Gen F
genF32 = do
    let (minF, maxF) = (-999999 :: Float, 999999.0 :: Float)
    f <- Gen.float $ Range.linearFrac minF maxF
    pure $ F32 f

genF64 :: Gen F
genF64 = do
    let (minD, maxD) = (-999999.0 :: Double, 999999.0 :: Double)
    f <- Gen.double $ Range.linearFrac minD maxD
    pure $ F64 f

genPrecursor :: Gen Precursor
genPrecursor = do
    mass <- Gen.double $ Range.linearFrac (-999999 :: Double) (999999 :: Double)
    charge <- Gen.int $ Range.linear (-999999 :: Int) (999999 :: Int)
    pure $ Precursor mass charge
    
encodeAsBinary :: [F] -> BL.ByteString
encodeAsBinary xs = runPut $ putValues xs

putValues :: [F] -> Put
putValues = mapM_ putValue

putValue :: F -> Put
putValue (F32 f) = putFloatle f
putValue (F64 f) = putDoublele f

serializeSpectrum :: Spectrum -> BL.ByteString
serializeSpectrum spec =
    let ident = spec^.spId
        mz = B64.encode $ encodeAsBinary $ spec^.spMz
        intensity = B64.encode $ encodeAsBinary $ spec^.spIntensity
        precMass = spec^.spPrecursor.pcMass
        precCharge = spec^.spPrecursor.pcCharge
    in BL.pack $ printf
    "<mzML>\
        \<run>\
            \<spectrumList>\
                \<spectrum id=\"%s\">\
                    \<precursorList count=\"1\">\
                                \<precursor>\
                                    \<isolationWindow>\
                                        \<cvParam accession=\"MS:1000827\" cvRef=\"MS\" name=\"isolation window target m/z\" unitAccession=\"MS:1000040\" unitCvRef=\"MS\" unitName=\"m/z\" value=\"763.92539\" />\
                                    \</isolationWindow>\
                                    \<selectedIonList count=\"1\">\
                                        \<selectedIon>\
                                            \<cvParam accession=\"MS:1000744\" cvRef=\"MS\" name=\"selected ion m/z\" unitAccession=\"MS:1000040\" unitCvRef=\"MS\" unitName=\"m/z\" value=\"%s\" />\
                                            \<cvParam accession=\"MS:1000041\" cvRef=\"MS\" name=\"charge state\" value=\"%s\" />\
                                        \</selectedIon>\
                                    \</selectedIonList>\
                                    \<activation>\
                                        \<cvParam accession=\"MS:1000044\" cvRef=\"MS\" name=\"dissociation method\" />\
                                    \</activation>\
                                \</precursor>\
                            \</precursorList>\
                    \<binaryDataArrayList>\
                        \<binaryDataArray>\
                            \<cvParam accession=\"MS:1000514\" name=\"m/z array\" />\
                            \<cvParam accession=\"MS:1000523\" name=\"64-bit float\" />\
                            \<binary>%s</binary>\
                        \</binaryDataArray>\
                        \<binaryDataArray>\
                            \<cvParam accession=\"MS:1000515\" name=\"intensity array\" />\
                            \<cvParam accession=\"MS:1000521\" name=\"32-bit float\" />\
                            \<binary>%s</binary>\
                        \</binaryDataArray>\
                    \</binaryDataArrayList>\
                \</spectrum>\
            \</spectrumList>\
        \</run>\
    \</mzML>"
    ident
    (show precMass)
    (show precCharge)
    (BL.unpack mz)
    (BL.unpack intensity)

