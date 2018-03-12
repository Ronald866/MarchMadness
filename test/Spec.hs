{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Bracket
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bracket" $ do
        it "should pick a game winner" $ do
            pickWinnerBasic (Game ub nc) `shouldBe` nc
        it "should pick winners from a list" $ do
            pickWinners [Game ub nc, Game bona az] pickWinnerBasic `shouldBe` [nc, az]

ub = Team {name="UB", seed=10, ppg=80, opg=75, rank=40}
nc = Team {name="NC", seed=3, ppg=80, opg=75, rank=20}
bona = Team {name="Bona", seed=11, ppg=80, opg=75, rank=30}
az = Team {name="AZ", seed=4, ppg=80, opg=75, rank=18}
