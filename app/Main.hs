module Main (main) where

import Data.Yaml.Config
import qualified Config as Cfg
import qualified Filtering as F
import Lens.Micro
import qualified Fasta
import qualified MzML.Parser as MzML
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Cfg.Config
    let fastaFile = config^.Cfg.input.Cfg.proteinFile
    let spectrumFile = config^.Cfg.input.Cfg.spectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent
    putStrLn "exited"

