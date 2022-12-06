module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML.Parser as MzML
import Domain (spId, spMz, spIntensity, Spectrum (..))
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.Light
import Data.Binary.Put
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Get
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Either
import GHC.Float (double2Float, float2Double)

data EncodingType = Float | Double
data DataType = Float32 Float | Float64 Double deriving Show
data Spec = Spec { id :: String, vals1 :: [DataType], vals2 :: [DataType] } deriving Show

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput . ciProteinFile
    let spectrumFile = config ^. cfgInput . ciSpectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent
    print (head spectra)
    let floats = [3.8, 8.1, 1238.3987, 3.99999872987] :: [Float]
    let doubles = [3.8, 8.1, 1238.3987, 3.99999872987] :: [Double]
    let spec1 = Spec "Hello" (map float2DataType floats) (map double2DataType doubles)
    let spec2 = Spec "Hello" (map float2DataType floats) (map float2DataType floats)
        specs = [spec1, spec2]
    print specs
    putStrLn "exited"

float2DataType :: Float -> DataType
float2DataType = Float32

double2DataType :: Double -> DataType
double2DataType = Float64

encodeAsBinaryD :: [Double] -> EncodingType -> BL.ByteString
encodeAsBinaryD xs t = B64.encode (runPut (putValuesD xs t))

encodeAsBinaryR :: [Rational] -> EncodingType -> BL.ByteString
encodeAsBinaryR xs t = B64.encode (runPut (putValuesR xs t))

putValuesD :: [Double] -> EncodingType -> Put
putValuesD xs Float = mapM_ (putFloatle . double2Float) xs
putValuesD xs Double = mapM_ putDoublele xs

putValuesR :: [Rational] -> EncodingType -> Put
putValuesR xs Float = mapM_ (putFloatle . fromRational) xs
putValuesR xs Double = mapM_ (putDoublele . fromRational) xs

runGetFloat :: BL.ByteString -> [Float]
runGetFloat = runGet (decodeBytesToFloating getFloatle)

runGetDouble :: BL.ByteString -> [Double]
runGetDouble = runGet (decodeBytesToFloating getDoublele)

decodeBytesToFloating :: Floating a => Get a -> Get [a]
decodeBytesToFloating decoder = do
    emp <- isEmpty
    if emp
    then return []
    else do val <- decoder
            vals <- decodeBytesToFloating decoder
            return (val:vals)

