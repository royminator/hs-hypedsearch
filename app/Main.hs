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

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput . ciProteinFile
    let spectrumFile = config ^. cfgInput . ciSpectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent
    -- print (head spectra)
    let floats = [3.8, 8.1, 1238.3987, 3.99999872987] :: [Float]
    let doubles = [3.8, 8.1, 1238.3987, 3.99999872987] :: [Double]
    let fr = map toRational floats
        fd = map float2Double floats
    print fd
    let f = encodeAsBinaryD fd Float
        d = encodeAsBinaryD fd Double
    print f
    print d
    let f' = runGetFloat $ B64.decodeLenient f
    print f'
    
    let f'' = encodeAsBinaryD (map float2Double f') Float
    print f''

    putStrLn "exited"

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

