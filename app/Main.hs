module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput ^. ciProteinFile
    let spectrum_file = config ^. cfgInput ^. ciSpectrumFile

    fasta <- Fasta.readFasta fastaFile
    spectrum <- MzML.readMzML spectrum_file
    print $ head spectrum
    putStrLn "exited"

