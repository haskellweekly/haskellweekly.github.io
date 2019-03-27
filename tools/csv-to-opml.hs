#!/usr/bin/env stack
-- stack --resolver lts-13.0 script
{-# OPTIONS_GHC -Weverything -Wno-implicit-prelude -Wno-missing-exported-signatures -Wno-safe #-}

-- | This script is meant to convert a CSV file of Haskell Weekly authors into
-- an OPML file. The OPML file can then be imported into your feed reader of
-- choice. You can get a CSV file of Haskell Weekly authors from Airtable at:
-- <https://airtable.com/shrPtsG8SLq5swdsM>.
module Main
  ( main
  )
where

import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment

main :: IO ()
main = do
  [input, output] <- Environment.getArgs
  contents <- readFile input
  writeFile output . unlines $ mconcat
    [ [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      , "<opml version=\"1.0\">"
      , "<head>"
      , "<title>Haskell Weekly</title>"
      , "<dateCreated>2001-01-01 01:01:01 +0000</dateCreated>"
      , "</head>"
      , "<body>"
      ]
    , Maybe.mapMaybe toOutline . drop 1 $ lines contents
    , ["</body>", "</opml>"]
    ]

toOutline :: String -> Maybe String
toOutline line = case breakOn ',' line of
  name : site : feed : _ -> if null feed
    then Nothing
    else Just $ mconcat
      [ "<outline type=\"rss\" text=\""
      , escape name
      , "\" description=\""
      , escape name
      , "\" htmlUrl=\""
      , escape site
      , "\" xmlUrl=\""
      , escape feed
      , "\" />"
      ]
  _ -> Nothing

breakOn :: (Eq a, Show a) => a -> [a] -> [[a]]
breakOn separator list = if null list
  then []
  else case break (== separator) list of
    (chunk, rest) -> chunk : breakOn separator (drop 1 rest)

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar c = case c of
  '"' -> "&quot;"
  '\'' -> "&apos;"
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  _ -> [c]
