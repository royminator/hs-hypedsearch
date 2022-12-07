module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML.Parser as MzML
import qualified MzML.Internal as MzI
import Domain
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.Light
import Data.Binary.Put
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Get
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Either
import GHC.Float (double2Float, float2Double)

main :: IO ()
main = do
    config <- loadYamlSettings ["config.yaml"] [] useEnv :: IO Config
    let fastaFile = config ^. cfgInput . ciProteinFile
    let spectrumFile = config ^. cfgInput . ciSpectrumFile

    fastaContent <- readFile fastaFile
    let fasta = Fasta.parseFasta fastaContent

    mzMLContent <- BL.readFile spectrumFile
    let spectra = MzML.parseMzML mzMLContent

    {-let floats = map F32 [3983.389, 87.47, 567.38]
        enc = encodeAsBinary floats
        bin = MzI.BinaryData enc MzI.Float
    let decoded = MzI.decodeBinaryData bin
    print decoded-}
    putStrLn "exited"

