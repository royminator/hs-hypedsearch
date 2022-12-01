module Main (main) where

import Data.Yaml.Config
import Config
import Lens.Micro
import qualified Fasta
import qualified MzML
import Domain (spId, spMz, spIntensity, Spectrum (..))
import qualified Data.ByteString.Lazy.Char8 as BL

import Text.XML.Light
import Data.Binary.Put
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Get
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Either
import GHC.Float (double2Float)

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
    -- print $ head spectra ^. spMz
    let spec = Spectrum "id" [8.3] [8.2]
        ser = serializeSpectrum spec
        s = BL.unpack ser
    putStrLn ("ser: " ++ s)
    let ps = MzML.parseMzML ser
    print ps
    print $ cmpSpec (head ps) spec 1e-4
    putStrLn "exited"

cmpSpec :: Spectrum -> Spectrum -> Double -> Bool
cmpSpec lhs rhs epsilon =
    let eqLen = (length (lhs ^. spMz) == length (rhs ^. spMz)) &&
            (length (lhs ^. spIntensity) == length (rhs ^. spIntensity))
        eqId = (lhs ^. spId) == (rhs ^. spId)
    in if eqLen && eqId
        then
            let eqMz = cmpDoubles (lhs ^. spMz) (rhs ^. spMz) epsilon
                eqInt = cmpDoubles (lhs ^. spIntensity) (rhs ^. spIntensity) epsilon
            in eqMz && eqInt
        else False


cmpDoubles :: [Double] -> [Double] -> Double -> Bool
cmpDoubles [] [] _ = True
cmpDoubles (l:ls) (r:rs) epsilon =
    if cmpDouble l r epsilon
    then cmpDoubles ls rs epsilon
    else False

cmpDouble :: Double -> Double -> Double -> Bool
cmpDouble lhs rhs epsilon =
    abs (lhs - rhs) <= epsilon
    
serializeSpectrum :: Spectrum -> BL.ByteString
serializeSpectrum = BL.pack . ppElement . spectrum2Xml

spectrum2Xml :: Spectrum -> Element
spectrum2Xml spectrum =
    let eBinaryMz = Element (unqual "binary") [] [createBinaryContent (spectrum ^. spMz) Double] Nothing
        cvParamsMz = map Elem
            [ Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000514", Attr (unqual "name") "m/z array"] [] Nothing
            , Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000523", Attr (unqual "name") "64-bit float"] [] Nothing
            ]
        eBinaryArrayMz = Element (unqual "binaryDataArray") [] (cvParamsMz ++ [Elem eBinaryMz]) Nothing

        eBinaryInt = Element (unqual "binary") [] [createBinaryContent (spectrum ^. spIntensity) Float] Nothing
        cvParamsInt = map Elem
            [ Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000515", Attr (unqual "name") "intensity array"] [] Nothing
            , Element (unqual "cvParam") [Attr (unqual "accession") "MS:1000521", Attr (unqual "name") "32-bit float"] [] Nothing
            ]
        eBinaryArrayInt = Element (unqual "binaryDataArray") [] (cvParamsInt ++ [Elem eBinaryInt]) Nothing

        eBinaryArrayList = Element (unqual "binaryDataArrayList") [] (map Elem [eBinaryArrayMz, eBinaryArrayInt]) Nothing
        eSpectrum = Element (unqual "spectrum") [Attr (unqual "id") (spectrum ^. spId)] [Elem eBinaryArrayList] Nothing
        eSpectrumList = Element (unqual "spectrumList") [] [Elem eSpectrum] Nothing
        eRun = Element (unqual "run") [] [Elem eSpectrumList] Nothing
    in Element (unqual "mzML") [] [Elem eRun] Nothing

createBinaryContent :: [Double] -> EncodingType -> Content
createBinaryContent xs t = Text $ CData CDataRaw (BL.unpack (encodeAsBinary xs t)) Nothing

encodeAsBinary :: [Double] -> EncodingType -> BL.ByteString
encodeAsBinary xs t = B64.encode (runPut (putValues xs t))

putValues :: [Double] -> EncodingType -> Put
putValues xs Float = mapM_ (putFloatle . double2Float) xs
putValues xs Double = mapM_ putDoublele xs

