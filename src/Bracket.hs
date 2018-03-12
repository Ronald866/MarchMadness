module Bracket where

data Team = Team
  {
    name   :: String
  , seed   :: Int
  , ppg    :: Int
  , opg    :: Int
  , rank   :: Int
  } deriving (Eq, Show)

data Game = Game Team Team

type GameWinnerStrategy = Game -> Team

pickWinnerBasic:: GameWinnerStrategy
pickWinnerBasic (Game teamA teamB)
  | rank teamA < rank teamB = teamA
  | rank teamB < rank teamA = teamB
  | otherwise               = teamA

pickWinners:: [Game] -> GameWinnerStrategy -> [Team]
pickWinners gms winStrategy = map winStrategy gms
