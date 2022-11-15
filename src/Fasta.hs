{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module Fasta
    ( readFasta
    , FastaReport (..)
    , sequences
    , Sequence (..)
    , seqDescr
    ) where

import Domain
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data Sequence a = Sequence
    { _seqDescr :: String
    , _seqData :: [a]
    } deriving (Show)

newtype FastaReport = FastaReport
    { _sequences :: [Sequence AA]
    } deriving (Show)

makeLenses ''FastaReport
makeLenses ''Sequence

readFasta :: FilePath -> IO FastaReport
readFasta path = do
    content <- readFile path
    return $ foldl parseFile (FastaReport []) (lines content)

parseFile :: FastaReport -> String -> FastaReport
parseFile rep line = do
    let newRep = addSeqIfNew rep line
        updated = parseLine (last (newRep ^. sequences)) line
    set (sequences . _last) updated newRep

addSeqIfNew :: FastaReport -> String -> FastaReport
addSeqIfNew rep ('>':_) = over sequences (++ [Sequence "" []]) rep
addSeqIfNew rep _ = rep

parseLine :: Sequence AA -> String -> Sequence AA
parseLine _ ('>':cs) = Sequence cs []
parseLine s line =
    let amino = aminoFromStr line
    in over seqData (++ amino) s

aminoFromStr :: String -> [AA]
aminoFromStr = map (read . pure :: Char -> AA)

