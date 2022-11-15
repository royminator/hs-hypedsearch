module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput ^. ciProteinFile
    fasta <- Fasta.readFasta fastaFile
    print fastaFile
    putStrLn (show fasta)

