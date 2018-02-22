{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

import qualified CMark
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.Read as Read

main :: IO ()
main = do
  let
    input = "content" :: FilePath
    output = "_site" :: FilePath

  -- Clean the output directory to avoid stale files.
  Directory.removePathForcibly output

  -- Create the expected hierarchy in the output directory.
  mapM_
    createDirectoryAt
    [ [output]
    , [output, "images"]
    , [output, "issues"]
    , [output, "surveys"]
    ]

  -- Copy over static files.
  mapM_
    (copyFileAt input output)
    [ ["images", "favicon.ico"]
    , ["images", "twitter-card.png"]
    ]

  -- Read includes and inject them into the context.
  form <- readFileAt [input, "includes", "form.html"]
  logo <- readFileAt [input, "includes", "logo.svg"]
  script <- readFileAt [input, "includes", "script.js"]
  style <- readFileAt [input, "includes", "style.css"]
  let
    context :: Context
    context =
      [ ("", "$")
      , ("baseUrl", "https://haskellweekly.news")
      , ("form", form)
      , ("logo", logo)
      , ("script", script)
      , ("style", style)
      ]

  -- Read templates.
  atomEntryTemplate <- readFileAt [input, "templates", "atom-entry.xml"]
  baseTemplate <- readFileAt [input, "templates", "base.html"]
  issueTemplate <- readFileAt [input, "templates", "issue.html"]
  rssItemTemplate <- readFileAt [input, "templates", "rss-item.xml"]
  snippetTemplate <- readFileAt [input, "templates", "snippet.html"]
  surveyTemplate <- readFileAt [input, "templates", "survey.html"]

  -- Read page templates.
  advertisingTemplate <- readFileAt [input, "pages", "advertising.html"]
  atomTemplate <- readFileAt [input, "pages", "atom.xml"]
  indexTemplate <- readFileAt [input, "pages", "index.html"]
  rssTemplate <- readFileAt [input, "pages", "rss.xml"]

  -- Read survey templates.
  surveyFiles <- listDirectoryAt [input, "surveys"]
  surveysByYear <- getSurveysByYear input surveyFiles

  -- Load issues.
  issueFiles <- listDirectoryAt [input, "issues"]
  issuesByNumber <- getIssuesByNumber input issueFiles

  -- Parse issues.
  let
    issues = sortIssues (Maybe.mapMaybe parseIssue issuesByNumber)
    recentIssues = take 7 issues

  -- Create issue pages.
  mapM_ (createIssue output baseTemplate issueTemplate context) issues

  -- Create feeds.
  do
    contents <- renderAtom atomTemplate atomEntryTemplate context recentIssues
    writeFileAt [output, "haskell-weekly.atom"] contents
  do
    contents <- renderRss rssTemplate rssItemTemplate context recentIssues
    writeFileAt [output, "haskell-weekly.rss"] contents

  -- Create advertising page.
  do
    contents <- renderAdvertising baseTemplate advertisingTemplate context
    writeFileAt [output, "advertising.html"] contents

  -- Create survey pages.
  mapM_ (createSurvey output baseTemplate surveyTemplate context) surveysByYear

  -- Create home page.
  do
    contents <- renderIndex
      baseTemplate
      indexTemplate
      snippetTemplate
      context
      issues
    writeFileAt [output, "index.html"] contents

getSurveysByYear :: FilePath -> [FilePath] -> IO [(Integer, Text)]
getSurveysByYear input =
  mapM (getSurvey input)
    . Maybe.mapMaybe (Read.readMaybe . FilePath.takeBaseName)
    . filter (hasExtension "html")

getSurvey :: FilePath -> Integer -> IO (Integer, Text)
getSurvey input year = do
  template <- readFileAt
    [input, "surveys", FilePath.addExtension (show year) "html"]
  pure (year, template)

getIssuesByNumber :: FilePath -> [FilePath] -> IO [(Integer, Text)]
getIssuesByNumber input =
  mapM (getIssue input)
    . Maybe.mapMaybe (Read.readMaybe . FilePath.takeBaseName)
    . filter (hasExtension "markdown")

getIssue :: FilePath -> Integer -> IO (Integer, Text)
getIssue input number = do
  contents <- readFileAt
    [input, "issues", FilePath.addExtension (show number) "markdown"]
  pure (number, contents)

createIssue :: FilePath -> Text -> Text -> Context -> Issue -> IO ()
createIssue output baseTemplate issueTemplate context issue = do
  let number = show (issueNumber issue)
  contents <- renderIssue baseTemplate issueTemplate context issue
  writeFileAt [output, "issues", FilePath.addExtension number "html"] contents

createSurvey :: FilePath -> Text -> Text -> Context -> (Integer, Text) -> IO ()
createSurvey output baseTemplate surveyTemplate context (year, template) = do
  contents <- renderSurvey baseTemplate surveyTemplate template context year
  writeFileAt
    [output, "surveys", FilePath.addExtension (show year) "html"]
    contents

-- Types

data Issue = Issue
  { issueNumber :: Integer
  , issueDay :: Time.Day
  , issueContents :: Text
  }

type Context = [(Text, Text)]

data Piece
  = Literal Text
  | Variable Text

-- Business helpers

commonMark :: Text -> Text
commonMark = CMark.commonmarkToHtml [CMark.optNormalize, CMark.optSmart]

escapeHtml :: Text -> Text
escapeHtml = Text.replace "<" "&lt;" . Text.replace "&" "&amp;"

getDay :: Text -> Maybe Time.Day
getDay meta = case Text.words meta of
  ["<!--", day, "-->"] -> parseDay "%Y-%m-%d" day
  _ -> Nothing

isoDay :: Time.Day -> Text
isoDay = formatDay "%Y-%m-%dT00:00:00Z"

issueContext :: Issue -> Context
issueContext issue = [("number", showText (issueNumber issue))]

issueSummary :: Monad m => Issue -> m Text
issueSummary =
  renderTemplate
      "Issue $number$ of Haskell Weekly, a free email newsletter about the \
      \Haskell programming language."
    . issueContext

issueTitle :: Monad m => Issue -> m Text
issueTitle = renderTemplate "Issue $number$" . issueContext

issueUrl :: Monad m => Issue -> m Text
issueUrl = renderTemplate "/issues/$number$.html" . issueContext

lastUpdated :: [Issue] -> Time.Day
lastUpdated =
  Maybe.fromMaybe (Time.fromGregorian 1970 1 1)
    . Maybe.listToMaybe
    . map issueDay

pageTitle :: Monad m => Maybe Text -> m Text
pageTitle maybeTitle = case maybeTitle of
  Nothing -> pure "Haskell Weekly"
  Just title -> renderTemplate "$title$ :: Haskell Weekly" [("title", title)]

parseIssue :: (Integer, Text) -> Maybe Issue
parseIssue (number, contents) = do
  let (meta, body) = Text.breakOn "\n" contents
  day <- getDay meta
  Just Issue
    { issueNumber = number
    , issueDay = day
    , issueContents = commonMark body
    }

prettyDay :: Time.Day -> Text
prettyDay = formatDay "%B %e %Y"

rfcDay :: Time.Day -> Text
rfcDay = formatDay "%a, %d %b %Y 00:00:00 GMT"

sortIssues :: [Issue] -> [Issue]
sortIssues = List.sortBy (Ord.comparing (Ord.Down . issueDay))

surveyContext :: Integer -> Context
surveyContext year = [("year", showText year)]

surveySummary :: Monad m => Integer -> m Text
surveySummary =
  renderTemplate
      "The $year$ survey of Haskell users by Haskell Weekly, a free email \
      \newsletter about the Haskell programming language."
    . surveyContext

surveyTitle :: Monad m => Integer -> m Text
surveyTitle = renderTemplate "$year$ survey" . surveyContext

surveyUrl :: Monad m => Integer -> m Text
surveyUrl = renderTemplate "/surveys/$year$.html" . surveyContext

-- Rendering helpers

renderAdvertising :: Monad m => Text -> Text -> Context -> m Text
renderAdvertising template advertisingTemplate context = do
  body <- renderTemplate advertisingTemplate context
  title <- pageTitle (Just "Advertising")
  renderTemplate
    template
    ( context
    ++ [ ("body", body)
       , ("summary", "Information about advertising with Haskell Weekly.")
       , ("title", title)
       , ("url", "/advertising.html")
       ]
    )

renderAtom :: Monad m => Text -> Text -> Context -> [Issue] -> m Text
renderAtom template entryTemplate context issues = do
  entries <- mapM (renderAtomEntry entryTemplate context) issues
  renderTemplate
    template
    ( context
    ++ [ ("entries", mconcat entries)
       , ("updated", isoDay (lastUpdated issues))
       ]
    )

renderAtomEntry :: Monad m => Text -> Context -> Issue -> m Text
renderAtomEntry template context issue = renderTemplate
  template
  ( context
  ++ [ ("content", escapeHtml (issueContents issue))
     , ("number", showText (issueNumber issue))
     , ("updated", isoDay (issueDay issue))
     ]
  )

renderIndex :: Monad m => Text -> Text -> Text -> Context -> [Issue] -> m Text
renderIndex baseTemplate template snippetTemplate context issues = do
  snippets <- mapM (renderSnippet snippetTemplate context) issues
  body <- renderTemplate template (("issues", mconcat snippets) : context)
  let
    summary :: Text
    summary
      = "Haskell Weekly is a free email newsletter about the Haskell \
        \programming language. Each issue features several hand-picked links \
        \to interesting content about Haskell from around the web."
  title <- pageTitle Nothing
  renderTemplate
    baseTemplate
    ( context
    ++ [("body", body), ("summary", summary), ("title", title), ("url", "")]
    )

renderIssue :: Monad m => Text -> Text -> Context -> Issue -> m Text
renderIssue baseTemplate issueTemplate context issue = do
  partialTitle <- issueTitle issue
  body <- renderTemplate
    issueTemplate
    ( context
    ++ [ ("body", issueContents issue)
       , ("date", prettyDay (issueDay issue))
       , ("title", partialTitle)
       ]
    )
  summary <- issueSummary issue
  url <- issueUrl issue
  title <- pageTitle (Just partialTitle)
  renderTemplate
    baseTemplate
    ( context
    ++ [("body", body), ("summary", summary), ("title", title), ("url", url)]
    )

renderPiece :: Monad m => Context -> Piece -> m Text
renderPiece context piece = case piece of
  Literal text -> pure text
  Variable name -> case lookup name context of
    Nothing -> fail ("unknown variable: " ++ show name)
    Just value -> pure value

renderPieces :: Monad m => Context -> [Piece] -> m Text
renderPieces context pieces = do
  rendered <- mapM (renderPiece context) pieces
  pure (mconcat rendered)

renderRss :: Monad m => Text -> Text -> Context -> [Issue] -> m Text
renderRss template itemTemplate context issues = do
  items <- mapM (renderRssItem itemTemplate context) issues
  renderTemplate template (("items", mconcat items) : context)

renderRssItem :: Monad m => Text -> Context -> Issue -> m Text
renderRssItem template context issue = do
  title <- issueTitle issue
  url <- issueUrl issue
  renderTemplate
    template
    ( context
    ++ [ ("description", escapeHtml (issueContents issue))
       , ("pubDate", rfcDay (issueDay issue))
       , ("title", title)
       , ("url", url)
       ]
    )

renderSnippet :: Monad m => Text -> Context -> Issue -> m Text
renderSnippet template context issue = do
  title <- issueTitle issue
  url <- issueUrl issue
  renderTemplate
    template
    ( context
    ++ [("date", prettyDay (issueDay issue)), ("title", title), ("url", url)]
    )

renderSurvey :: Monad m => Text -> Text -> Text -> Context -> Integer -> m Text
renderSurvey baseTemplate surveyTemplate template context year = do
  partialBody <- renderTemplate template context
  partialTitle <- surveyTitle year
  body <- renderTemplate
    surveyTemplate
    (context ++ [("body", partialBody), ("title", partialTitle)])
  summary <- surveySummary year
  title <- pageTitle (Just partialTitle)
  url <- surveyUrl year
  renderTemplate
    baseTemplate
    ( context
    ++ [("body", body), ("summary", summary), ("title", title), ("url", url)]
    )

renderTemplate :: Monad m => Text -> Context -> m Text
renderTemplate template context = renderPieces context (toPieces template)

toPieces :: Text -> [Piece]
toPieces = textsToPieces . Text.splitOn "$"

textsToPieces :: [Text] -> [Piece]
textsToPieces chunks = case chunks of
  [] -> []
  [text] -> [Literal text]
  text:name:rest -> Literal text : Variable name : textsToPieces rest

-- Generic helpers

copyFileAt :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFileAt input output path = Directory.copyFile
  (FilePath.joinPath (input : path))
  (FilePath.joinPath (output : path))

createDirectoryAt :: [FilePath] -> IO ()
createDirectoryAt =
  Directory.createDirectoryIfMissing True . FilePath.joinPath

formatDay :: Text -> Time.Day -> Text
formatDay format =
  Text.pack . Time.formatTime Time.defaultTimeLocale (Text.unpack format)

hasExtension :: Text -> FilePath -> Bool
hasExtension extension =
  (== '.' : Text.unpack extension) . FilePath.takeExtension

listDirectoryAt :: [FilePath] -> IO [FilePath]
listDirectoryAt = Directory.listDirectory . FilePath.joinPath

parseDay :: Text -> Text -> Maybe Time.Day
parseDay format =
  Time.parseTimeM False Time.defaultTimeLocale (Text.unpack format)
    . Text.unpack

readFileAt :: [FilePath] -> IO Text
readFileAt path = do
  handle <- IO.openFile (FilePath.joinPath path) IO.ReadMode
  IO.hSetEncoding handle IO.utf8
  Text.hGetContents handle

showText :: Show a => a -> Text
showText = Text.pack . show

writeFileAt :: [FilePath] -> Text -> IO ()
writeFileAt path contents = do
  handle <- IO.openFile (FilePath.joinPath path) IO.WriteMode
  IO.hSetEncoding handle IO.utf8
  Text.hPutStr handle contents
  IO.hFlush handle
