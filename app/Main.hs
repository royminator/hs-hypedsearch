module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML
import qualified Data.ByteString.Lazy.Char8 as BL
import Domain (spId, spMz)
import Data.Binary.Get
import Data.ByteString.Base64.Lazy

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput ^. ciProteinFile
    let spectrum_file = config ^. cfgInput ^. ciSpectrumFile

    fasta <- Fasta.readFasta fastaFile
    spectra <- MzML.readMzML spectrum_file
    let spectrum = head spectra
        b = BL.pack $ spectrum ^. spMz
        bq = decode b
    case bq of
        Right bs -> do
            putStrLn $ "spectrum ID: " ++ spectrum ^. spId
            print $ runGet getMz bs
        Left e ->
            putStrLn $ "error: " ++ e
    putStrLn "exited"

{-
getMz :: Get Double
getMz = getDoublebe
-}

getMz :: Get [Double]
getMz = do
    empty <- isEmpty
    if empty
        then return []
        else do d <- getDoublele
                ds <- getMz
                return (d:ds)
