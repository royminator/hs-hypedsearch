module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML
import Domain (spId, spMz, spIntensity)
import qualified Data.ByteString.Lazy as BL

import Text.XML.Light
import Data.Binary.Put
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Get
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Either

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput . ciProteinFile
    let spectrumFile = config ^. cfgInput . ciSpectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent
    -- print $ head spectra ^. spMz
    let ds = [2.3, 2842.9, 9871.8] :: [Double]
    let bs = (runGet getValues) . B64.decodeLenient . B64.encode . runPut $ putValues ds
    -- BL.putStrLn $ bs
    print bs
    putStrLn "exited"

putValues :: [Double] -> Put
putValues xs = mapM_ putDoublele xs

getValues :: Get [Double]
getValues = do
    emp <- isEmpty
    if emp
    then return []
    else do val <- getDoublele
            vals <- getValues
            return (val:vals)

