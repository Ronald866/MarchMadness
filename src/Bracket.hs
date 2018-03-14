{-# LANGUAGE OverloadedStrings #-}
module Bracket where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data Team = Team
  {
    ppg       :: !Float
  , name      :: !String
  , op_fg_pct :: Float
  , region    :: String
  , fg_pct    :: Float
  , tov       :: Int
  , op_tov    :: Int
  , op_pg     :: Float
  , seed      :: Int
  , gp        :: Int
  } deriving (Eq, Show)

instance FromNamedRecord Team where
  parseNamedRecord r = Team <$>
    r .: "ppg"
    <*> r.: "name"
    <*> r.: "op_fg_pct"
    <*> r.: "region"
    <*> r.: "fg_pct"
    <*> r.: "tov"
    <*> r.: "op_tov"
    <*> r.: "opg"
    <*> r.: "seed"
    <*> r.: "gs"

data Game = Game Team Team

type GameWinnerStrategy = Game -> Team

pickWinnerBasic:: GameWinnerStrategy
pickWinnerBasic (Game teamA teamB)
  | ppg teamA < ppg teamB = teamA
  | ppg teamB < ppg teamA = teamB
  | otherwise               = teamA

pickWinners:: [Game] -> GameWinnerStrategy -> [Team]
pickWinners gms winStrategy = map winStrategy gms

parseTeams :: IO (Either String (V.Vector Team))
parseTeams = do
  csvData <- BL.readFile "stats.csv"
  case decodeByName csvData of
    Left err -> return (Left err)
    Right (_, v) -> return (Right v)

