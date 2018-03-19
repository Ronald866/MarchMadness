{-# LANGUAGE OverloadedStrings #-}
module Bracket where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as DM

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
  } deriving (Eq)

instance Show Team where
  show team = name team
  
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

type GameId = Int
data Game = Game Team Team GameId  deriving (Eq, Show)

type GameWinnerStrategy = Game -> Team
type Seed = Int
type TeamName = String
type Region = String

pickWinnerBasic :: GameWinnerStrategy
pickWinnerBasic (Game teamA teamB _)
  | ppg teamA < ppg teamB = teamB
  | ppg teamB < ppg teamA = teamA
  | otherwise               = teamA

avgTeamPtsToOppDef :: Team -> Team -> Float
avgTeamPtsToOppDef t1 t2 = ((ppg t1) + (op_pg t2)) / 2

simplePicks = pickWinners (getGames $ createBracket teams) pickWinnerBasic
mediumPicks = pickWinners (getGames $ createBracket teams) pickWinnerMedium

pickWinnerMedium :: GameWinnerStrategy
pickWinnerMedium (Game teamA teamB _)
  | avgTeamPtsToOppDef teamA teamB > avgTeamPtsToOppDef teamB teamA = teamA
  | otherwise               = teamB

predictRound :: [Game] -> GameWinnerStrategy -> DM.Map Int [Team]
predictRound gms str = foldl (\map game -> DM.insertWith (++) (nextGame game) ([str game]) map) DM.empty gms
  where numOfGames = length gms 
        nextGame = \(Game t1 t2 id) -> case (even id) of
                                         True -> id + numOfGames - 1
                                         False -> id + numOfGames

pickWinners :: [Game] -> GameWinnerStrategy -> [Team]
pickWinners gms winStrategy = map winStrategy gms

sortTeams :: Team -> Team -> Ordering
sortTeams t1 t2
  | seed t1 < seed t2 = LT
  | otherwise = GT

getBracketTeamInfo :: Team -> (Seed, Region, TeamName)
getBracketTeamInfo team = (seed team, region team, name team)

getGames :: [Maybe Game] -> [Game]
getGames maybeGames = catMaybes maybeGames

type TeamMap = DM.Map (Int, String) Team

createBracket :: [Team] -> [Maybe Game]
createBracket teams = map (\(seedRegion, gmId) -> createGame (fst seedRegion) (snd seedRegion) gmId teamMap) (zip ((,) <$> seedMatchups <*> regions) [1..])
  where teamMap = createTeamsMap teams

createGame :: (Int, Int) -> String -> GameId-> TeamMap -> Maybe Game
createGame seeds region gameId teamMap = do
  teamOne <- DM.lookup (fst seeds, region) teamMap
  teamTwo <- DM.lookup (snd seeds, region) teamMap
  return $ Game teamOne teamTwo gameId

createTeamsMap :: [Team] -> DM.Map (Int, String) Team
createTeamsMap teams = foldl (\map team -> DM.insert (seed team, region team) team map) DM.empty teams

parseTeams :: IO (Either String (V.Vector Team))
parseTeams = do
  csvData <- BL.readFile "stats.csv"
  case decodeByName csvData of
    Left err -> return (Left err)
    Right (_, v) -> return (Right v)

seedMatchups = [(1, 16), (2, 15), (3, 14), (4, 13), (5, 12), (6, 11), (7, 10), (8, 9)]
seedsWith
regions = ["East", "South", "Midwest", "West"]
teams =  [Team {ppg = 67.5, name = "virginia", op_fg_pct = 0.375, region = "South", fg_pct = 0.461, tov = 283, op_tov = 420, op_pg = 53.4, seed = 1, gp = 33},Team {ppg = 87.1, name = "villanova", op_fg_pct = 0.434, region = "East", fg_pct = 0.504, tov = 352, op_tov = 445, op_pg = 70.9, seed = 1, gp = 34},Team {ppg = 84.7, name = "duke", op_fg_pct = 0.401, region = "Midwest", fg_pct = 0.493, tov = 416, op_tov = 396, op_pg = 69.6, seed = 2, gp = 33},Team {ppg = 81.1, name = "purdue", op_fg_pct = 0.41, region = "East", fg_pct = 0.497, tov = 363, op_tov = 412, op_pg = 65.6, seed = 2, gp = 34},Team {ppg = 81.0, name = "michigan-state", op_fg_pct = 0.366, region = "Midwest", fg_pct = 0.504, tov = 435, op_tov = 329, op_pg = 64.8, seed = 3, gp = 33},Team {ppg = 74.5, name = "cincinnati", op_fg_pct = 0.37, region = "South", fg_pct = 0.453, tov = 389, op_tov = 503, op_pg = 57.1, seed = 2, gp = 34},Team {ppg = 82.0, name = "north-carolina", op_fg_pct = 0.422, region = "West", fg_pct = 0.46, tov = 422, op_tov = 406, op_pg = 73.1, seed = 2, gp = 35},Team {ppg = 81.5, name = "kansas", op_fg_pct = 0.422, region = "Midwest", fg_pct = 0.498, tov = 397, op_tov = 442, op_pg = 70.9, seed = 1, gp = 34},Team {ppg = 75.2, name = "texas-tech", op_fg_pct = 0.403, region = "East", fg_pct = 0.464, tov = 412, op_tov = 499, op_pg = 64.7, seed = 3, gp = 33},Team {ppg = 74.5, name = "michigan", op_fg_pct = 0.43, region = "West", fg_pct = 0.471, tov = 320, op_tov = 450, op_pg = 63.5, seed = 3, gp = 35},Team {ppg = 84.5, name = "gonzaga", op_fg_pct = 0.405, region = "West", fg_pct = 0.503, tov = 384, op_tov = 417, op_pg = 67.1, seed = 4, gp = 34},Team {ppg = 74.2, name = "tennessee", op_fg_pct = 0.41, region = "South", fg_pct = 0.439, tov = 403, op_tov = 461, op_pg = 66.4, seed = 3, gp = 33},Team {ppg = 79.6, name = "west-virginia", op_fg_pct = 0.425, region = "East", fg_pct = 0.434, tov = 385, op_tov = 561, op_pg = 69.0, seed = 5, gp = 34},Team {ppg = 75.8, name = "ohio-state", op_fg_pct = 0.415, region = "West", fg_pct = 0.485, tov = 378, op_tov = 393, op_pg = 66.7, seed = 5, gp = 32},Team {ppg = 84.3, name = "xavier", op_fg_pct = 0.431, region = "West", fg_pct = 0.491, tov = 410, op_tov = 375, op_pg = 74.5, seed = 1, gp = 33},Team {ppg = 83.4, name = "auburn", op_fg_pct = 0.425, region = "Midwest", fg_pct = 0.439, tov = 393, op_tov = 488, op_pg = 73.3, seed = 4, gp = 32},Team {ppg = 73.3, name = "clemson", op_fg_pct = 0.41, region = "Midwest", fg_pct = 0.453, tov = 387, op_tov = 383, op_pg = 65.8, seed = 5, gp = 32},Team {ppg = 83.0, name = "wichita-state", op_fg_pct = 0.419, region = "East", fg_pct = 0.478, tov = 370, op_tov = 370, op_pg = 71.3, seed = 4, gp = 32},Team {ppg = 76.1, name = "florida", op_fg_pct = 0.429, region = "East", fg_pct = 0.434, tov = 315, op_tov = 442, op_pg = 69.4, seed = 6, gp = 32},Team {ppg = 77.4, name = "houston", op_fg_pct = 0.397, region = "West", fg_pct = 0.466, tov = 401, op_tov = 438, op_pg = 64.9, seed = 6, gp = 33},Team {ppg = 80.9, name = "arizona", op_fg_pct = 0.431, region = "South", fg_pct = 0.505, tov = 417, op_tov = 396, op_pg = 71.2, seed = 4, gp = 34},Team {ppg = 75.0, name = "texas-am", op_fg_pct = 0.403, region = "West", fg_pct = 0.457, tov = 434, op_tov = 360, op_pg = 69.8, seed = 7, gp = 32},Team {ppg = 76.7, name = "kentucky", op_fg_pct = 0.406, region = "South", fg_pct = 0.47, tov = 442, op_tov = 407, op_pg = 70.3, seed = 5, gp = 34},Team {ppg = 79.1, name = "butler", op_fg_pct = 0.45, region = "East", fg_pct = 0.472, tov = 369, op_tov = 467, op_pg = 72.8, seed = 10, gp = 33},Team {ppg = 83.0, name = "texas-christian", op_fg_pct = 0.459, region = "Midwest", fg_pct = 0.499, tov = 406, op_tov = 403, op_pg = 75.9, seed = 6, gp = 32},Team {ppg = 84.3, name = "creighton", op_fg_pct = 0.433, region = "South", fg_pct = 0.497, tov = 358, op_tov = 376, op_pg = 74.2, seed = 8, gp = 32},Team {ppg = 73.3, name = "missouri", op_fg_pct = 0.407, region = "West", fg_pct = 0.453, tov = 453, op_tov = 354, op_pg = 68.1, seed = 8, gp = 32},Team {ppg = 79.0, name = "seton-hall", op_fg_pct = 0.437, region = "Midwest", fg_pct = 0.469, tov = 410, op_tov = 410, op_pg = 73.3, seed = 8, gp = 32},Team {ppg = 81.8, name = "florida-state", op_fg_pct = 0.421, region = "West", fg_pct = 0.474, tov = 403, op_tov = 437, op_pg = 74.5, seed = 9, gp = 31},Team {ppg = 79.7, name = "virginia-tech", op_fg_pct = 0.425, region = "East", fg_pct = 0.498, tov = 381, op_tov = 416, op_pg = 71.8, seed = 8, gp = 32},Team {ppg = 71.7, name = "texas", op_fg_pct = 0.422, region = "South", fg_pct = 0.438, tov = 390, op_tov = 418, op_pg = 68.2, seed = 10, gp = 33},Team {ppg = 85.2, name = "oklahoma", op_fg_pct = 0.445, region = "Midwest", fg_pct = 0.464, tov = 414, op_tov = 409, op_pg = 81.6, seed = 10, gp = 31},Team {ppg = 74.2, name = "miami-fl", op_fg_pct = 0.43, region = "South", fg_pct = 0.464, tov = 351, op_tov = 402, op_pg = 68.0, seed = 6, gp = 31},Team {ppg = 77.5, name = "san-diego-state", op_fg_pct = 0.418, region = "West", fg_pct = 0.461, tov = 382, op_tov = 436, op_pg = 67.9, seed = 11, gp = 32},Team {ppg = 81.1, name = "arkansas", op_fg_pct = 0.428, region = "East", fg_pct = 0.477, tov = 374, op_tov = 476, op_pg = 75.5, seed = 7, gp = 34},Team {ppg = 72.4, name = "alabama", op_fg_pct = 0.413, region = "East", fg_pct = 0.458, tov = 481, op_tov = 449, op_pg = 70.0, seed = 9, gp = 34},Team {ppg = 67.5, name = "syracuse", op_fg_pct = 0.396, region = "Midwest", fg_pct = 0.418, tov = 414, op_tov = 424, op_pg = 64.5, seed = 11, gp = 33},Team {ppg = 72.4, name = "kansas-state", op_fg_pct = 0.429, region = "South", fg_pct = 0.472, tov = 379, op_tov = 473, op_pg = 67.9, seed = 9, gp = 33},Team {ppg = 83.1, name = "nevada", op_fg_pct = 0.43, region = "South", fg_pct = 0.468, tov = 338, op_tov = 442, op_pg = 72.9, seed = 7, gp = 34},Team {ppg = 81.2, name = "north-carolina-state", op_fg_pct = 0.465, region = "Midwest", fg_pct = 0.47, tov = 390, op_tov = 480, op_pg = 74.5, seed = 9, gp = 32},Team {ppg = 76.2, name = "rhode-island", op_fg_pct = 0.451, region = "Midwest", fg_pct = 0.459, tov = 343, op_tov = 508, op_pg = 67.9, seed = 7, gp = 32},Team {ppg = 73.7, name = "providence", op_fg_pct = 0.443, region = "West", fg_pct = 0.442, tov = 424, op_tov = 455, op_pg = 72.7, seed = 10, gp = 34},Team {ppg = 83.5, name = "arizona-state", op_fg_pct = 0.439, region = "Midwest", fg_pct = 0.464, tov = 333, op_tov = 463, op_pg = 75.3, seed = 11, gp = 31},Team {ppg = 75.9, name = "new-mexico-state", op_fg_pct = 0.392, region = "Midwest", fg_pct = 0.46, tov = 418, op_tov = 455, op_pg = 63.8, seed = 12, gp = 33},Team {ppg = 76.4, name = "davidson", op_fg_pct = 0.437, region = "South", fg_pct = 0.483, tov = 306, op_tov = 342, op_pg = 67.6, seed = 12, gp = 32},Team {ppg = 72.4, name = "loyola-il", op_fg_pct = 0.412, region = "South", fg_pct = 0.507, tov = 404, op_tov = 434, op_pg = 62.2, seed = 11, gp = 33},Team {ppg = 84.9, name = "south-dakota-state", op_fg_pct = 0.429, region = "West", fg_pct = 0.47, tov = 340, op_tov = 362, op_pg = 74.1, seed = 12, gp = 34},Team {ppg = 78.9, name = "murray-state", op_fg_pct = 0.41, region = "East", fg_pct = 0.485, tov = 368, op_tov = 394, op_pg = 65.5, seed = 12, gp = 31},Team {ppg = 73.5, name = "north-carolina-greensboro", op_fg_pct = 0.405, region = "West", fg_pct = 0.447, tov = 468, op_tov = 518, op_pg = 62.4, seed = 13, gp = 34},Team {ppg = 78.1, name = "montana", op_fg_pct = 0.429, region = "West", fg_pct = 0.471, tov = 410, op_tov = 508, op_pg = 68.7, seed = 14, gp = 33},Team {ppg = 77.9, name = "st-bonaventure", op_fg_pct = 0.421, region = "East", fg_pct = 0.456, tov = 367, op_tov = 459, op_pg = 71.0, seed = 11, gp = 32},Team {ppg = 75.3, name = "georgia-state", op_fg_pct = 0.393, region = "South", fg_pct = 0.463, tov = 386, op_tov = 493, op_pg = 67.3, seed = 15, gp = 34},Team {ppg = 75.1, name = "college-of-charleston", op_fg_pct = 0.435, region = "Midwest", fg_pct = 0.46, tov = 318, op_tov = 391, op_pg = 68.8, seed = 13, gp = 33},Team {ppg = 81.1, name = "stephen-f-austin", op_fg_pct = 0.428, region = "East", fg_pct = 0.488, tov = 527, op_tov = 678, op_pg = 68.1, seed = 14, gp = 34},Team {ppg = 81.1, name = "bucknell", op_fg_pct = 0.423, region = "Midwest", fg_pct = 0.472, tov = 429, op_tov = 428, op_pg = 72.9, seed = 14, gp = 34},Team {ppg = 67.4, name = "radford", op_fg_pct = 0.428, region = "East", fg_pct = 0.426, tov = 413, op_tov = 441, op_pg = 64.4, seed = 16, gp = 34},Team {ppg = 84.8, name = "buffalo", op_fg_pct = 0.425, region = "South", fg_pct = 0.474, tov = 426, op_tov = 474, op_pg = 75.9, seed = 13, gp = 34},Team {ppg = 84.3, name = "marshall", op_fg_pct = 0.441, region = "East", fg_pct = 0.469, tov = 449, op_tov = 485, op_pg = 78.8, seed = 13, gp = 34},Team {ppg = 72.1, name = "wright-state", op_fg_pct = 0.414, region = "South", fg_pct = 0.436, tov = 446, op_tov = 519, op_pg = 65.7, seed = 14, gp = 34},Team {ppg = 76.4, name = "pennsylvania", op_fg_pct = 0.413, region = "Midwest", fg_pct = 0.46, tov = 379, op_tov = 398, op_pg = 68.7, seed = 16, gp = 32},Team {ppg = 79.8, name = "iona", op_fg_pct = 0.458, region = "Midwest", fg_pct = 0.462, tov = 385, op_tov = 452, op_pg = 76.2, seed = 15, gp = 33},Team {ppg = 82.6, name = "lipscomb", op_fg_pct = 0.446, region = "West", fg_pct = 0.456, tov = 480, op_tov = 493, op_pg = 77.5, seed = 15, gp = 32},Team {ppg = 73.2, name = "cal-state-fullerton", op_fg_pct = 0.413, region = "East", fg_pct = 0.47, tov = 438, op_tov = 404, op_pg = 70.0, seed = 15, gp = 31},Team {ppg = 77.6, name = "texas-southern", op_fg_pct = 0.454, region = "West", fg_pct = 0.441, tov = 428, op_tov = 398, op_pg = 79.7, seed = 16, gp = 34},Team {ppg = 73.9, name = "maryland-baltimore-county", op_fg_pct = 0.44, region = "South", fg_pct = 0.445, tov = 411, op_tov = 504, op_pg = 69.5, seed = 16, gp = 34}]
