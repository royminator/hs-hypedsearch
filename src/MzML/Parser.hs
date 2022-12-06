module MzML.Parser
    ( parseMzML
    ) where

import Text.XML.Light
import Domain
import MzML.Internal
import qualified Data.ByteString.Lazy.Char8 as BL

parseMzML :: BL.ByteString -> [Spectrum]
parseMzML contents =
    let spectrumList = parseXMLDoc contents
            >>= filterElementName (qNameEq "spectrumList")
    in maybe [] parseSpectrumList spectrumList
