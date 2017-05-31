{-# LANGUAGE OverloadedStrings #-}

module HaskellWeekly (main) where

import Data.Maybe (mapMaybe)
import Hakyll

import qualified Text.HTML.TagSoup as Html

-- Main

main :: IO ()
main = hakyllWith configuration rules

-- Configuration

configuration :: Configuration
configuration = defaultConfiguration
  { destinationDirectory = "_hakyll/site"
  , providerDirectory = "content"
  , storeDirectory = "_hakyll/cache"
  , tmpDirectory = "_hakyll/tmp"
  }

-- Rules

rules :: Rules ()
rules = do
  match "templates/*" templateRules
  match "images/*" imageRules
  match "issues/*" issueRules
  create ["haskell-weekly.atom"] (feedRules renderAtom)
  create ["haskell-weekly.rss"] (feedRules renderRss)
  match "pages/index.html" indexRules

templateRules :: Rules ()
templateRules = compile templateBodyCompiler

imageRules :: Rules ()
imageRules = do
  route idRoute
  compile getResourceLBS

issueRules :: Rules ()
issueRules = do
  route (setExtension ".html")
  compile (pandocCompiler
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/issue.html" issueContext
    >>= loadAndApplyTemplate "templates/base.html" issueContext
    >>= relativizeUrls)

feedRules :: RenderFeed -> Rules ()
feedRules render = do
  route idRoute
  compile (do
    issues <- loadIssues (Just 8)
    render feedConfiguration feedContext issues)

indexRules :: Rules ()
indexRules = do
  route (constRoute "index.html")
  compile (do
    issues <- loadIssues Nothing
    let context = indexContext issues
    getResourceBody
      >>= applyAsTemplate context
      >>= loadAndApplyTemplate "templates/base.html" context
      >>= relativizeUrls)

-- Contexts

baseContext :: Context String
baseContext = mconcat [field "summary" summarize, defaultContext]

issueContext :: Context String
issueContext = mconcat
  [ boolField "hasTitle" (const True)
  , dateField "date" "%B %-e %Y"
  , dateField "isoDate" "%Y-%m-%d"
  , baseContext
  ]

feedContext :: Context String
feedContext = mconcat [bodyField "description", issueContext]

indexContext :: [Item String] -> Context String
indexContext issues = mconcat
  [ boolField "hasTitle" (const False)
  , listField "issues" issueContext (pure issues)
  , baseContext
  ]

-- Helpers

type RenderFeed = FeedConfiguration
  -> Context String
  -> [Item String]
  -> Compiler (Item String)

extractText :: Html.Tag String -> Maybe String
extractText tag = case tag of
  Html.TagText x -> Just x
  _ -> Nothing

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedAuthorEmail = "info@haskellweekly.news"
  , feedAuthorName = "Haskell Weekly"
  , feedDescription = "A weekly Haskell newsletter."
  , feedRoot = "https://haskellweekly.news"
  , feedTitle = "Haskell Weekly"
  }

loadIssues :: Maybe Int -> Compiler [Item String]
loadIssues limit = do
  issues <- loadAllSnapshots "issues/*" "content"
  sortedIssues <- recentFirst issues
  pure (maybeTake limit sortedIssues)

maybeTake :: Maybe Int -> [a] -> [a]
maybeTake = maybe id take

summarize :: Item String -> Compiler String
summarize = pure
  . takeWords 32
  . unwords
  . mapMaybe extractText
  . Html.parseTags
  . itemBody

takeWords :: Int -> String -> String
takeWords n = unwords . take n . words
