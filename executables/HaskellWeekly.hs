{-# LANGUAGE OverloadedStrings #-}

import qualified Hakyll as H hiding (relativizeUrls)
import qualified RelativizeUrls as H
import qualified Text.HTML.TagSoup as TS


main :: IO ()
main = do
    H.hakyllWith configuration rules


configuration :: H.Configuration
configuration = do
    H.defaultConfiguration
        { H.destinationDirectory = "_hakyll/site"
        , H.providerDirectory = "content"
        , H.storeDirectory = "_hakyll/cache"
        , H.tmpDirectory = "_hakyll/tmp"
        }


rules :: H.Rules ()
rules = do
    templateRules
    imageRules
    styleRules
    issueRules
    feedRules
    pageRules


templateRules :: H.Rules ()
templateRules = do
    H.match "templates/*" (do
        H.compile H.templateBodyCompiler)


imageRules :: H.Rules ()
imageRules = do
    H.match "images/*" (do
        H.route H.idRoute
        H.compile H.getResourceLBS)


styleRules :: H.Rules ()
styleRules = do
    H.match "styles/*" (do
        H.route H.idRoute
        H.compile H.compressCssCompiler)


issueRules :: H.Rules ()
issueRules = do
    H.match "issues/*" (do
        H.route (H.setExtension ".html")
        H.compile (do
            H.pandocCompiler
                >>= H.saveSnapshot "content"
                >>= H.loadAndApplyTemplate "templates/issue.html" issueContext
                >>= H.loadAndApplyTemplate "templates/base.html" defaultContext
                >>= H.relativizeUrls))


issueContext :: H.Context String
issueContext = do
    mconcat
        [ H.dateField "date" "%B %-e %Y"
        , defaultContext
        ]


defaultContext :: H.Context String
defaultContext = do
    mconcat
        [ H.field "summary" (\item -> do
            let body = H.itemBody item
            let tags = TS.parseTags body
            let extractText tag = case tag of
                    TS.TagText x -> x
                    _ -> ""
            let text = map extractText tags
            let summary = unwords (take 27 (words (unwords text)))
            pure summary)
        , H.defaultContext
        ]


feedRules :: H.Rules ()
feedRules = do
    let limit = Just 8

    H.create ["haskell-weekly.atom"] (do
        H.route H.idRoute
        H.compile (do
            issues <- loadIssues limit
            H.renderAtom feedConfiguration feedContext issues))

    H.create ["haskell-weekly.rss"] (do
        H.route H.idRoute
        H.compile (do
            issues <- loadIssues limit
            H.renderRss feedConfiguration feedContext issues))


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
feedContext = do
    mconcat
        [ H.bodyField "description"
        , issueContext
        ]


feedConfiguration :: H.FeedConfiguration
feedConfiguration = do
    H.FeedConfiguration
        { H.feedAuthorEmail = "info@haskellweekly.news"
        , H.feedAuthorName = "Haskell Weekly"
        , H.feedDescription = "A weekly Haskell newsletter."
        , H.feedRoot = "http://haskellweekly.news"
        , H.feedTitle = "Haskell Weekly"
        }


pageRules :: H.Rules ()
pageRules = do
    H.match "pages/*" (do
        H.route (H.customRoute (drop 6 . H.toFilePath))
        H.compile (do
            issues <- loadIssues Nothing
            let context = mconcat
                    [ H.listField "issues" issueContext (pure issues)
                    , defaultContext
                    ]

            H.getResourceBody
                >>= H.applyAsTemplate context
                >>= H.loadAndApplyTemplate "templates/base.html" defaultContext
                >>= H.relativizeUrls))
