{-# LANGUAGE OverloadedStrings #-}

module HaskellWeekly where

import qualified Hakyll as H
import qualified Text.HTML.TagSoup as TS

main :: IO ()
main = H.hakyllWith configuration rules

configuration :: H.Configuration
configuration =
  H.defaultConfiguration
  { H.destinationDirectory = "_hakyll/site"
  , H.providerDirectory = "content"
  , H.storeDirectory = "_hakyll/cache"
  , H.tmpDirectory = "_hakyll/tmp"
  }

rules :: H.Rules ()
rules = do
  H.match "templates/*" (H.compile H.templateBodyCompiler)
  H.match "images/*" imageRules
  H.match "styles/*" styleRules
  H.match "issues/*" issueRules
  H.create ["haskell-weekly.atom"] (feedRules H.renderAtom)
  H.create ["haskell-weekly.rss"] (feedRules H.renderRss)
  H.match "pages/index.html" indexRules

imageRules :: H.Rules ()
imageRules = do
  H.route H.idRoute
  H.compile H.getResourceLBS

styleRules :: H.Rules ()
styleRules = do
  H.route H.idRoute
  H.compile H.compressCssCompiler

issueRules :: H.Rules ()
issueRules = do
  H.route (H.setExtension ".html")
  H.compile
    (H.pandocCompiler >>= H.saveSnapshot "content" >>=
     H.loadAndApplyTemplate "templates/issue.html" issueContext >>=
     H.loadAndApplyTemplate "templates/base.html" issueContext >>=
     H.relativizeUrls)

issueContext :: H.Context String
issueContext =
  mconcat
    [ H.boolField "hasTitle" (const True)
    , H.dateField "date" "%B %-e %Y"
    , H.dateField "isoDate" "%Y-%m-%d"
    , defaultContext
    ]

defaultContext :: H.Context String
defaultContext = mconcat [H.field "summary" summarize, H.defaultContext]

summarize :: H.Item String -> H.Compiler String
summarize item = do
  let body = H.itemBody item
  let tags = TS.parseTags body
  let text = map extractText tags
  let summary = takeWords 27 (unwords text)
  pure summary

extractText :: TS.Tag String -> String
extractText tag =
  case tag of
    TS.TagText x -> x
    _ -> ""

takeWords :: Int -> String -> String
takeWords n = unwords . take n . words

feedRules
  :: (H.FeedConfiguration -> H.Context String -> [H.Item String] -> H.Compiler (H.Item String))
  -> H.Rules ()
feedRules render = do
  let limit = Just 8
  H.route H.idRoute
  H.compile
    (do issues <- loadIssues limit
        render feedConfiguration feedContext issues)

loadIssues :: Maybe Int -> H.Compiler [H.Item String]
loadIssues maybeLimit = do
  allIssues <- H.loadAllSnapshots "issues/*" "content"
  allSortedIssues <- H.recentFirst allIssues
  case maybeLimit of
    Nothing -> pure allSortedIssues
    Just limit -> do
      let someSortedIssues = take limit allSortedIssues
      pure someSortedIssues

feedContext :: H.Context String
feedContext = mconcat [H.bodyField "description", issueContext]

feedConfiguration :: H.FeedConfiguration
feedConfiguration =
  H.FeedConfiguration
  { H.feedAuthorEmail = "info@haskellweekly.news"
  , H.feedAuthorName = "Haskell Weekly"
  , H.feedDescription = "A weekly Haskell newsletter."
  , H.feedRoot = "https://haskellweekly.news"
  , H.feedTitle = "Haskell Weekly"
  }

indexRules :: H.Rules ()
indexRules = do
  H.route (H.constRoute "index.html")
  H.compile
    (do issues <- loadIssues Nothing
        let context =
              mconcat
                [ H.listField "issues" issueContext (pure issues)
                , defaultContext
                ]
        H.getResourceBody >>= H.applyAsTemplate context >>=
          H.loadAndApplyTemplate "templates/base.html" indexContext >>=
          H.relativizeUrls)

indexContext :: H.Context String
indexContext = mconcat [H.boolField "hasTitle" (const False), defaultContext]
