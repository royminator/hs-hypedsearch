{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module Fasta
    ( module Fasta
    ) where

import Domain
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data Sequence a = Sequence
    { _seqDescr :: String
    , _seqData :: [a]
    } deriving (Show)

newtype Report = Report
    { _frSequences :: [Sequence AminoAcid]
    } deriving (Show)

makeLenses ''Report
makeLenses ''Sequence

parseFasta :: String -> Report
parseFasta content =
    foldl parseFile (Report []) (lines content)

parseFile :: Report -> String -> Report
parseFile rep line = do
    let newRep = addSeqIfNew rep line
        updated = parseLine (last (newRep ^. frSequences)) line
    set (frSequences . _last) updated newRep

addSeqIfNew :: Report -> String -> Report
addSeqIfNew rep ('>':_) = over frSequences (++ [Sequence "" []]) rep
addSeqIfNew rep _ = rep

parseLine :: Sequence AminoAcid -> String -> Sequence AminoAcid
parseLine _ ('>':cs) = Sequence cs []
parseLine s line =
    let amino = aminoFromStr line
    in over seqData (++ amino) s

aminoFromStr :: String -> [AminoAcid]
aminoFromStr = map (read . pure :: Char -> AminoAcid)

