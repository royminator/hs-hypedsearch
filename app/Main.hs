module Main (main) where

import Data.Yaml.Config
import Config
import qualified Filtering as F
import Lens.Micro
import qualified Fasta
import qualified MzML.Parser as MzML
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput . ciProteinFile
    let spectrumFile = config ^. cfgInput . ciSpectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent
    print $ F.applyFilter (F.takePeaks 25) (head spectra)
    putStrLn "exited"

