module MzML.Parser
    ( parseMzML
    , Spectrum
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import           MzML.Internal
import           Text.XML.Light

parseMzML :: BL.ByteString -> [Spectrum]
parseMzML contents =
    let spectrumList = parseXMLDoc contents
            >>= filterElementName (qNameEq "spectrumList")
    in maybe [] parseSpectrumList spectrumList

