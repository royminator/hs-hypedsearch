module MzMLSpec
    ( mzMLProps
    ) where

import Prelude hiding (id)
import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8  as BL
import           Domain
import           GHC.Float                   (double2Float, float2Double)
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Lens.Micro
import           MzML.Internal
import qualified MzML.Parser                 as MzML
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Printf

mzMLProps :: [TestTree]
mzMLProps = [ testProperty "encoded and decoded [Float] equals input" prop_encodeDecodeFloat
            , testProperty "verify parsed then serialized spectrum equals input" prop_serializeParseMzML
            ]

prop_encodeDecodeFloat :: Property
prop_encodeDecodeFloat = property $ do
    floats <- forAll $ Gen.list (Range.linear 0 100) genFloat
    let bin = BinaryData (encodeAsBinaryF floats) Float
    decodeBinaryData bin === Just (map float2Double floats)

prop_serializeParseMzML :: Property
prop_serializeParseMzML = property $ do
    spectrum <- forAll genSpectrum
    let serialized = serializeSpectrum spectrum
        parsed = head $ MzML.parseMzML serialized
    parsed === spectrum

genSpectrum :: Gen Spectrum
genSpectrum = do
    ident <- Gen.string (Range.linear 1 100) Gen.alphaNum
    mz <- Gen.list (Range.linear 1 100) genDouble
    intensity <- Gen.list (Range.linear 1 100) genFloat
    precursor <- genPrecursor
    pure $ Spectrum ident mz (map float2Double intensity) precursor

genDouble :: Gen Double
genDouble =
    let (minD, maxD) = (-100000.0 :: Double, 100000.0 :: Double)
    in Gen.double $ Range.linearFrac minD maxD

genFloat :: Gen Float
genFloat =
    let (minF, maxF) = (-100000.0 :: Float, 100000.0 :: Float)
    in Gen.float $ Range.linearFrac minF maxF

genPrecursor :: Gen Precursor
genPrecursor = do
    mass <- Gen.double $ Range.linearFrac (-10000.0 :: Double) (10000.0 :: Double)
    charge <- genCharge
    pure $ Precursor mass charge

genCharge :: Gen Charge
genCharge = do
    chargeInt <- Gen.int $ Range.linear (0 :: Int) (1 :: Int)
    pure $ int2Charge chargeInt
    where
        int2Charge 0 = Singly
        int2Charge 1 = Doubly

encodeAsBinaryF :: [Float] -> BL.ByteString
encodeAsBinaryF = runPut . mapM_ putFloatle

encodeAsBinaryD :: [Double] -> BL.ByteString
encodeAsBinaryD = runPut . mapM_ putDoublele

serializeSpectrum :: Spectrum -> BL.ByteString
serializeSpectrum spec =
    let ident = spec^.id
        masses = B64.encode $ encodeAsBinaryD $ spec^.mz
        abundances = B64.encode $ encodeAsBinaryF $ map double2Float $ spec^.abundance
        precMass = spec^.precursor.mass
        precCharge = charge2Int $ spec^.precursor.charge
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
    (BL.unpack masses)
    (BL.unpack abundances)
    where
        charge2Int Singly = 1
        charge2Int Doubly = 2

