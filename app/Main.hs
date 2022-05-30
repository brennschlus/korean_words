{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord (..), decodeByName, (.:))
import qualified Data.Vector as V
import qualified Data.Word8 as W
import System.Random (randomRIO)

main :: IO ()
main = do
  let fileName = "words.csv"
  csvData <- BL.readFile fileName
  let vectorWithCommas = BL.filter (isComma) csvData
  let length = (fromIntegral $ BL.length vectorWithCommas) - 2 :: Int
  randomNumber <- randomRIO (0, length)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ (V.slice (randomNumber) 1 v) $ \p ->
      putStrLn $ word p ++ " translates as " ++ translation p

data KoreanWord = KoreanWord
  { word :: !String,
    translation :: !String
  }
  deriving (Show)

instance FromNamedRecord KoreanWord where
  parseNamedRecord r = KoreanWord <$> r .: "word" <*> r .: "translation"

isComma :: W.Word8 -> Bool
isComma word = word == W._comma

-- https://docs.google.com/spreadsheets/d/16pRPe1aNRoQbcihSBkGs5La5b59gQ0bCq1-ss09pgFI/edit#gid=0
