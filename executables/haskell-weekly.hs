import Data.Function ((&))
import Data.Semigroup ((<>))

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
  let input = "content"
  let output = "_site"

  -- Clean the output directory to avoid stale files.
  Directory.removePathForcibly output

  -- Create the expected hierarchy in the output directory.
  mapM_ createDirectoryAt
    [ [output]
    , [output, "images"]
    , [output, "issues"]
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
  let context =
        [ (Text.pack "baseUrl", Text.pack "https://haskellweekly.news")
        , (Text.pack "form", form)
        , (Text.pack "logo", logo)
        , (Text.pack "script", script)
        , (Text.pack "style", style)
        ]

  -- Read templates.
  atomEntryTemplate <- readFileAt [input, "templates", "atom-entry.xml"]
  atomTemplate <- readFileAt [input, "templates", "atom.xml"]
  baseTemplate <- readFileAt [input, "templates", "base.html"]
  indexTemplate <- readFileAt [input, "templates", "index.html"]
  issueTemplate <- readFileAt [input, "templates", "issue.html"]
  rssItemTemplate <- readFileAt [input, "templates", "rss-item.xml"]
  rssTemplate <- readFileAt [input, "templates", "rss.xml"]
  snippetTemplate <- readFileAt [input, "templates", "snippet.html"]

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
  let issues = sortIssues (Maybe.mapMaybe parseIssue issuesByNumber)

  -- Create issue pages.
  Monad.forM_ issues (\ issue -> do
    let number = show (issueNumber issue)
    contents <- renderIssue baseTemplate issueTemplate context issue
    writeFileAt [output, "issues", FilePath.addExtension number "html"] contents)

  -- Create home page.
  do
    contents <- renderIndex baseTemplate indexTemplate snippetTemplate context issues
    writeFileAt [output, "index.html"] contents

  -- Create Atom feed.
  do
    contents <- renderAtom atomTemplate atomEntryTemplate context (take 7 issues)
    writeFileAt [output, "haskell-weekly.atom"] contents

  -- Create RSS feed.
  do
    contents <- renderRss rssTemplate rssItemTemplate context (take 7 issues)
    writeFileAt [output, "haskell-weekly.rss"] contents

-- Types

data Issue = Issue
  { issueNumber :: Word
  , issueDay :: Time.Day
  , issueContents :: Text.Text
  }

type Context = [(Text.Text, Text.Text)]

data Piece
  = Literal Text.Text
  | Variable Text.Text

-- Business helpers

commonMark :: Text.Text -> Text.Text
commonMark markdown =
  CMark.commonmarkToHtml [CMark.optNormalize, CMark.optSmart] markdown

escapeHtml :: Text.Text -> Text.Text
escapeHtml html =
  html
  & Text.replace (Text.pack "&") (Text.pack "&amp;")
  & Text.replace (Text.pack "<") (Text.pack "&lt;")

getDay :: Text.Text -> Maybe Time.Day
getDay meta =
  case Text.words meta of
    [open, day, close] -> if open == Text.pack "<!--" && close == Text.pack "-->"
      then parseDay day
      else Nothing
    _ -> Nothing

isoDay :: Time.Day -> Text.Text
isoDay day =
  Text.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT00:00:00Z" day)

issueTitle :: Issue -> Text.Text
issueTitle issue =
  Text.pack "Issue " <> Text.pack (show (issueNumber issue))

issueUrl :: Issue -> Text.Text
issueUrl issue =
  Text.concat [Text.pack "/issues/", Text.pack (show (issueNumber issue)), Text.pack ".html"]

lastUpdated :: [Issue] -> Time.Day
lastUpdated issues =
  issues
  & map issueDay
  & Maybe.listToMaybe
  & Maybe.fromMaybe (Time.fromGregorian 1970 1 1)

parseDay :: Text.Text -> Maybe Time.Day
parseDay day =
  Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" (Text.unpack day)

parseIssue :: (Word, Text.Text) -> Maybe Issue
parseIssue (number, contents) = do
  let (meta, body) = Text.breakOn (Text.pack "\n") contents
  day <- getDay meta
  Just Issue
    { issueNumber = number
    , issueDay = day
    , issueContents = commonMark body
    }

prettyDay :: Time.Day -> Text.Text
prettyDay day =
  Text.pack (Time.formatTime Time.defaultTimeLocale "%B %e %Y" day)

rfcDay :: Time.Day -> Text.Text
rfcDay day =
  Text.pack (Time.formatTime Time.defaultTimeLocale "%a, %d %b %Y 00:00:00 GMT" day)

sortIssues :: [Issue] -> [Issue]
sortIssues issues =
  List.sortBy (Ord.comparing (\ issue -> Ord.Down (issueDay issue))) issues

-- Rendering helpers

renderAtom :: Monad m => Text.Text -> Text.Text -> Context -> [Issue] -> m Text.Text
renderAtom template entryTemplate context issues = do
  entries <- mapM (renderAtomEntry entryTemplate context) issues
  renderTemplate template (context <>
    [ (Text.pack "entries", Text.concat entries)
    , (Text.pack "updated", isoDay (lastUpdated issues))
    ])

renderAtomEntry :: Monad m => Text.Text -> Context -> Issue -> m Text.Text
renderAtomEntry template context issue =
  renderTemplate template (context <>
    [ (Text.pack "content", escapeHtml (issueContents issue))
    , (Text.pack "number", Text.pack (show (issueNumber issue)))
    , (Text.pack "updated", isoDay (issueDay issue))
    ])

renderIndex :: Monad m => Text.Text -> Text.Text -> Text.Text -> Context -> [Issue] -> m Text.Text
renderIndex baseTemplate template snippetTemplate context issues = do
  snippets <- mapM (renderSnippet snippetTemplate context) issues
  body <- renderTemplate template ((Text.pack "issues", Text.concat snippets) : context)
  renderTemplate baseTemplate (context <>
    [ (Text.pack "body", body)
    , (Text.pack "summary", Text.pack "Haskell Weekly is a free email newsletter about the Haskell programming language. Each issue features several hand-picked links to interesting content about Haskell from around the web.")
    , (Text.pack "title", Text.pack "Haskell Weekly")
    , (Text.pack "url", Text.pack "")
    ])

renderIssue :: Monad m => Text.Text -> Text.Text -> Context -> Issue -> m Text.Text
renderIssue baseTemplate issueTemplate context issue = do
  let title = issueTitle issue
  body <- renderTemplate issueTemplate (context <>
    [ (Text.pack "body", issueContents issue)
    , (Text.pack "date", prettyDay (issueDay issue))
    , (Text.pack "title", title)
    ])
  renderTemplate baseTemplate (context <>
    [ (Text.pack "body", body)
    , (Text.pack "summary", Text.pack "Issue " <> Text.pack (show (issueNumber issue)) <> Text.pack " of Haskell Weekly.")
    , (Text.pack "title", title <> Text.pack " :: Haskell Weekly")
    , (Text.pack "url", issueUrl issue)
    ])

renderPiece :: Monad m => Context -> Piece -> m Text.Text
renderPiece context piece =
  case piece of
    Literal text -> pure text
    Variable name -> case lookup name context of
      Nothing -> fail ("unknown variable: " <> show name)
      Just value -> pure value

renderPieces :: Monad m => Context -> [Piece] -> m Text.Text
renderPieces context pieces = do
  rendered <- mapM (renderPiece context) pieces
  pure (Text.concat rendered)

renderRss :: Monad m => Text.Text -> Text.Text -> Context -> [Issue] -> m Text.Text
renderRss template itemTemplate context issues = do
  items <- mapM (renderRssItem itemTemplate context) issues
  renderTemplate template ((Text.pack "items", Text.concat items) : context)

renderRssItem :: Monad m => Text.Text -> Context -> Issue -> m Text.Text
renderRssItem template context issue =
  renderTemplate template (context <>
    [ (Text.pack "description", escapeHtml (issueContents issue))
    , (Text.pack "pubDate", rfcDay (issueDay issue))
    , (Text.pack "title", issueTitle issue)
    , (Text.pack "url", issueUrl issue)
    ])

renderSnippet :: Monad m => Text.Text -> Context -> Issue -> m Text.Text
renderSnippet template context issue =
  renderTemplate template (context <>
    [ (Text.pack "date", prettyDay (issueDay issue))
    , (Text.pack "title", issueTitle issue)
    , (Text.pack "url", issueUrl issue)
    ])

renderTemplate :: Monad m => Text.Text -> Context -> m Text.Text
renderTemplate template context =
  renderPieces context (toPieces template)

toPieces :: Text.Text -> [Piece]
toPieces template =
  let go chunks = case chunks of
        [] -> []
        [text] -> [Literal text]
        text : name : rest -> Literal text : Variable name : go rest
  in go (Text.splitOn (Text.pack "$") template)

-- Generic helpers

copyFileAt :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFileAt input output path =
  Directory.copyFile
    (FilePath.joinPath (input : path))
    (FilePath.joinPath (output : path))

createDirectoryAt :: [FilePath] -> IO ()
createDirectoryAt path =
  Directory.createDirectoryIfMissing True (FilePath.joinPath path)

hasExtension :: String -> FilePath -> Bool
hasExtension extension file =
  FilePath.takeExtension file == '.' : extension

listDirectoryAt :: [FilePath] -> IO [FilePath]
listDirectoryAt path =
  Directory.listDirectory (FilePath.joinPath path)

readFileAt :: [FilePath] -> IO Text.Text
readFileAt path = do
  handle <- IO.openFile (FilePath.joinPath path) IO.ReadMode
  IO.hSetEncoding handle IO.utf8
  Text.hGetContents handle

writeFileAt :: [FilePath] -> Text.Text -> IO ()
writeFileAt path contents = do
  handle <- IO.openFile (FilePath.joinPath path) IO.WriteMode
  IO.hSetEncoding handle IO.utf8
  Text.hPutStr handle contents
  IO.hFlush handle
