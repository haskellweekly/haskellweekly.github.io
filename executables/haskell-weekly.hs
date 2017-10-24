{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Data.Text (Text)

import qualified CMark
import qualified Control.Monad as Monad
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

  -- Configure input and output directories.
  let
    input :: FilePath
    input = "content"
    output :: FilePath
    output = "_site"

  -- Clean the output directory to avoid stale files.
  Directory.removePathForcibly output

  -- Create the expected hierarchy in the output directory.
  mapM_ createDirectoryAt
    [ [output]
    , [output, "images"]
    , [output, "issues"]
    , [output, "surveys"]
    ]

  -- Copy over static files.
  mapM_ (copyFileAt input output)
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
  surveysByYear <- surveyFiles
    & filter (hasExtension "html")
    & map FilePath.takeBaseName
    & Maybe.mapMaybe Read.readMaybe
    & mapM (\ year -> do
      template <- readFileAt [input, "surveys", FilePath.addExtension (show year) "html"]
      pure (year, template))

  -- Load issues.
  issueFiles <- listDirectoryAt [input, "issues"]
  issuesByNumber <- issueFiles
    & filter (hasExtension "markdown")
    & map FilePath.takeBaseName
    & Maybe.mapMaybe Read.readMaybe
    & mapM (\ number -> do
      contents <- readFileAt [input, "issues", FilePath.addExtension (show number) "markdown"]
      pure (number, contents))

  -- Parse issues.
  let
    issues = sortIssues (Maybe.mapMaybe parseIssue issuesByNumber)
    recentIssues = take 7 issues

  -- Create issue pages.
  Monad.forM_ issues (\ issue -> do
    let number = show (issueNumber issue)
    contents <- renderIssue baseTemplate issueTemplate context issue
    writeFileAt [output, "issues", FilePath.addExtension number "html"] contents)

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
  Monad.forM_ surveysByYear (\ (year, template) -> do
    contents <- renderSurvey baseTemplate surveyTemplate template context year
    writeFileAt [output, "surveys", FilePath.addExtension (show year) "html"] contents)

  -- Create home page.
  do
    contents <- renderIndex baseTemplate indexTemplate snippetTemplate context issues
    writeFileAt [output, "index.html"] contents

-- Types

data Issue = Issue
  { issueNumber :: Word
  , issueDay :: Time.Day
  , issueContents :: Text
  }

type Context = [(Text, Text)]

data Piece
  = Literal Text
  | Variable Text

-- Business helpers

commonMark :: Text -> Text
commonMark markdown =
  CMark.commonmarkToHtml [CMark.optNormalize, CMark.optSmart] markdown

escapeHtml :: Text -> Text
escapeHtml html =
  html
  & Text.replace "&" "&amp;"
  & Text.replace "<" "&lt;"

getDay :: Text -> Maybe Time.Day
getDay meta =
  case Text.words meta of
    ["<!--", day, "-->"] -> parseDay "%Y-%m-%d" day
    _ -> Nothing

isoDay :: Time.Day -> Text
isoDay day =
  formatDay "%Y-%m-%dT00:00:00Z" day

issueContext :: Issue -> Context
issueContext issue =
  [ ("number", showText (issueNumber issue))
  ]

issueSummary :: Issue -> Text
issueSummary issue =
  unsafeRenderTemplate
    "Issue $number$ of Haskell Weekly, a free email newsletter about the \
    \Haskell programming language."
    (issueContext issue)

issueTitle :: Issue -> Text
issueTitle issue =
  unsafeRenderTemplate
    "Issue $number$"
    (issueContext issue)

issueUrl :: Issue -> Text
issueUrl issue =
  unsafeRenderTemplate
    "/issues/$number$.html"
    (issueContext issue)

lastUpdated :: [Issue] -> Time.Day
lastUpdated issues =
  issues
  & map issueDay
  & Maybe.listToMaybe
  & Maybe.fromMaybe (Time.fromGregorian 1970 1 1)

pageTitle :: Maybe Text -> Text
pageTitle maybeTitle =
  case maybeTitle of
    Nothing -> "Haskell Weekly"
    Just title -> unsafeRenderTemplate
      "$title$ :: Haskell Weekly"
      [("title", title)]

parseIssue :: (Word, Text) -> Maybe Issue
parseIssue (number, contents) = do
  let (meta, body) = Text.breakOn "\n" contents
  day <- getDay meta
  Just Issue
    { issueNumber = number
    , issueDay = day
    , issueContents = commonMark body
    }

prettyDay :: Time.Day -> Text
prettyDay day =
  formatDay "%B %e %Y" day

rfcDay :: Time.Day -> Text
rfcDay day =
  formatDay "%a, %d %b %Y 00:00:00 GMT" day

sortIssues :: [Issue] -> [Issue]
sortIssues issues =
  List.sortBy (Ord.comparing (\ issue -> Ord.Down (issueDay issue))) issues

surveyContext :: Integer -> Context
surveyContext year =
  [ ("year", showText year)
  ]

surveySummary :: Monad m => Integer -> m Text
surveySummary year =
  renderTemplate
    "The $year$ survey of Haskell users by Haskell Weekly, a free email \
    \newsletter about the Haskell programming language."
    (surveyContext year)

surveyTitle :: Monad m => Integer -> m Text
surveyTitle year =
  renderTemplate
    "$year$ survey"
    (surveyContext year)

surveyUrl :: Monad m => Integer -> m Text
surveyUrl year =
  renderTemplate
    "/surveys/$year$.html"
    (surveyContext year)

-- Rendering helpers

renderAdvertising :: Monad m => Text -> Text -> Context -> m Text
renderAdvertising template advertisingTemplate context = do
  body <- renderTemplate advertisingTemplate context
  renderTemplate template (context ++
    [ ("body", body)
    , ("summary", "Information about advertising with Haskell Weekly.")
    , ("title", pageTitle (Just "Advertising"))
    , ("url", "/advertising.html")
    ])

renderAtom :: Monad m => Text -> Text -> Context -> [Issue] -> m Text
renderAtom template entryTemplate context issues = do
  entries <- mapM (renderAtomEntry entryTemplate context) issues
  renderTemplate template (context ++
    [ ("entries", mconcat entries)
    , ("updated", isoDay (lastUpdated issues))
    ])

renderAtomEntry :: Monad m => Text -> Context -> Issue -> m Text
renderAtomEntry template context issue =
  renderTemplate template (context ++
    [ ("content", escapeHtml (issueContents issue))
    , ("number", showText (issueNumber issue))
    , ("updated", isoDay (issueDay issue))
    ])

renderIndex :: Monad m => Text -> Text -> Text -> Context -> [Issue] -> m Text
renderIndex baseTemplate template snippetTemplate context issues = do
  snippets <- mapM (renderSnippet snippetTemplate context) issues
  body <- renderTemplate template (("issues", mconcat snippets) : context)
  let
    summary :: Text
    summary =
      "Haskell Weekly is a free email newsletter about the Haskell \
      \programming language. Each issue features several hand-picked links to \
      \interesting content about Haskell from around the web."
  renderTemplate baseTemplate (context ++
    [ ("body", body)
    , ("summary", summary)
    , ("title", pageTitle Nothing)
    , ("url", "")
    ])

renderIssue :: Monad m => Text -> Text -> Context -> Issue -> m Text
renderIssue baseTemplate issueTemplate context issue = do
  let title = issueTitle issue
  body <- renderTemplate issueTemplate (context ++
    [ ("body", issueContents issue)
    , ("date", prettyDay (issueDay issue))
    , ("title", title)
    ])
  renderTemplate baseTemplate (context ++
    [ ("body", body)
    , ("summary", issueSummary issue)
    , ("title", pageTitle (Just title))
    , ("url", issueUrl issue)
    ])

renderPiece :: Monad m => Context -> Piece -> m Text
renderPiece context piece =
  case piece of
    Literal text -> pure text
    Variable name -> case lookup name context of
      Nothing -> failText (unsafeRenderTemplate
        "unknown variable: $name$"
        [("name", showText name)])
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
renderRssItem template context issue =
  renderTemplate template (context ++
    [ ("description", escapeHtml (issueContents issue))
    , ("pubDate", rfcDay (issueDay issue))
    , ("title", issueTitle issue)
    , ("url", issueUrl issue)
    ])

renderSnippet :: Monad m => Text -> Context -> Issue -> m Text
renderSnippet template context issue =
  renderTemplate template (context ++
    [ ("date", prettyDay (issueDay issue))
    , ("title", issueTitle issue)
    , ("url", issueUrl issue)
    ])

renderSurvey :: Monad m => Text -> Text -> Text -> Context -> Integer -> m Text
renderSurvey baseTemplate surveyTemplate template context year = do
  partialBody <- renderTemplate template context
  partialTitle <- surveyTitle year
  body <- renderTemplate surveyTemplate (context ++
    [ ("body", partialBody)
    , ("title", partialTitle)
    ])
  summary <- surveySummary year
  let title = pageTitle (Just partialTitle)
  url <- surveyUrl year
  renderTemplate baseTemplate (context ++
    [ ("body", body)
    , ("summary", summary)
    , ("title", title)
    , ("url", url)
    ])

renderTemplate :: Monad m => Text -> Context -> m Text
renderTemplate template context =
  renderPieces context (toPieces template)

toPieces :: Text -> [Piece]
toPieces template =
  let
    go chunks = case chunks of
      [] -> []
      [text] -> [Literal text]
      text : name : rest -> Literal text : Variable name : go rest
  in go (Text.splitOn "$" template)

unsafeRenderTemplate :: Text -> Context -> Text
unsafeRenderTemplate template context =
  case renderTemplate template context of
    Left message -> error message
    Right text -> text

-- Generic helpers

copyFileAt :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFileAt input output path =
  Directory.copyFile
    (FilePath.joinPath (input : path))
    (FilePath.joinPath (output : path))

createDirectoryAt :: [FilePath] -> IO ()
createDirectoryAt path =
  Directory.createDirectoryIfMissing True (FilePath.joinPath path)

failText :: Monad m => Text -> m a
failText message =
  fail (Text.unpack message)

formatDay :: String -> Time.Day -> Text
formatDay format day =
  Text.pack (Time.formatTime Time.defaultTimeLocale format day)

hasExtension :: String -> FilePath -> Bool
hasExtension extension file =
  FilePath.takeExtension file == '.' : extension

listDirectoryAt :: [FilePath] -> IO [FilePath]
listDirectoryAt path =
  Directory.listDirectory (FilePath.joinPath path)

parseDay :: String -> Text -> Maybe Time.Day
parseDay format day =
  Time.parseTimeM False Time.defaultTimeLocale format (Text.unpack day)

readFileAt :: [FilePath] -> IO Text
readFileAt path = do
  handle <- IO.openFile (FilePath.joinPath path) IO.ReadMode
  IO.hSetEncoding handle IO.utf8
  Text.hGetContents handle

showText :: Show a => a -> Text
showText x =
  Text.pack (show x)

writeFileAt :: [FilePath] -> Text -> IO ()
writeFileAt path contents = do
  handle <- IO.openFile (FilePath.joinPath path) IO.WriteMode
  IO.hSetEncoding handle IO.utf8
  Text.hPutStr handle contents
  IO.hFlush handle
