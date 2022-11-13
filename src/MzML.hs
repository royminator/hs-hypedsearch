module MzML
    ( readMzML
    , printXML
    ) where

import System.IO
import Text.XML.Light
import Text.XML.Light.Input

data Spectrum = Spectrum
    { spMzRatios :: [Double]
    , spIntensity :: [Double]
    } deriving (Show)

readMzML :: FilePath -> IO [Spectrum]
readMzML path = do
    contents <- readFile path
    let xml = parseXML contents
    return []

printXML :: [Content] -> IO ()
printXML [] = return ()
printXML (x:xs) = do
    putStrLn $ show x
    printXML xs
    
