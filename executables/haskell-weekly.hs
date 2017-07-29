import qualified CMark
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

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
        [ ("baseUrl", "https://haskellweekly.news")
        , ("form", form)
        , ("logo", logo)
        , ("script", script)
        , ("style", style)
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
    |> filter (hasExtension "markdown")
    |> map FilePath.takeBaseName
    |> mapMaybe safeRead
    |> mapM (\ number -> do
      contents <- readFileAt [input, "issues", FilePath.addExtension (show number) "markdown"]
      pure (number, contents))

  -- Parse issues.
  let issues = sortIssues (mapMaybe parseIssue issuesByNumber)

  -- Create issue pages.
  forM_ issues (\ issue -> do
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
  , issueContents :: String
  }

type Context = [(String, String)]

-- Business helpers

commonMark :: String -> String
commonMark markdown =
  Text.unpack (CMark.commonmarkToHtml [CMark.optNormalize, CMark.optSmart] (Text.pack markdown))

escapeHtml :: String -> String
escapeHtml html =
  html
  |> replace '&' "&amp;"
  |> replace '<' "&lt;"

getDay :: String -> Maybe Time.Day
getDay meta =
  case words meta of
    ["<!--", day, "-->"] -> parseDay day
    _ -> Nothing

isoDay :: Time.Day -> String
isoDay day =
  Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT00:00:00Z" day

issueTitle :: Issue -> String
issueTitle issue =
  "Issue " ++ show (issueNumber issue)

issueUrl :: Issue -> String
issueUrl issue =
  concat ["/issues/", show (issueNumber issue), ".html"]

lastUpdated :: [Issue] -> Time.Day
lastUpdated issues =
  issues
  |> map issueDay
  |> safeHead
  |> withDefault (Time.fromGregorian 1970 1 1)

parseDay :: String -> Maybe Time.Day
parseDay day =
  Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" day

parseIssue :: (Word, String) -> Maybe Issue
parseIssue (number, contents) = do
  let (meta, body) = break (== '\n') contents
  day <- getDay meta
  Just Issue
    { issueNumber = number
    , issueDay = day
    , issueContents = commonMark body
    }

prettyDay :: Time.Day -> String
prettyDay day =
  Time.formatTime Time.defaultTimeLocale "%B %e %Y" day

rfcDay :: Time.Day -> String
rfcDay day =
  Time.formatTime Time.defaultTimeLocale "%a, %d %b %Y 00:00:00 GMT" day

sortIssues :: [Issue] -> [Issue]
sortIssues issues =
  List.sortBy (Ord.comparing (\ issue -> Ord.Down (issueDay issue))) issues

-- Rendering helpers

renderAtom :: Monad m => String -> String -> Context -> [Issue] -> m String
renderAtom template entryTemplate context issues = do
  entries <- mapM (renderAtomEntry entryTemplate context) issues
  renderTemplate template (context ++
    [ ("entries", concat entries)
    , ("updated", isoDay (lastUpdated issues))
    ])

renderAtomEntry :: Monad m => String -> Context -> Issue -> m String
renderAtomEntry template context issue =
  renderTemplate template (context ++
    [ ("content", escapeHtml (issueContents issue))
    , ("number", show (issueNumber issue))
    , ("updated", isoDay (issueDay issue))
    ])

renderIndex :: Monad m => String -> String -> String -> Context -> [Issue] -> m String
renderIndex baseTemplate template snippetTemplate context issues = do
  snippets <- mapM (renderSnippet snippetTemplate context) issues
  body <- renderTemplate template (("issues", concat snippets) : context)
  renderTemplate baseTemplate (context ++
    [ ("body", body)
    , ("summary", "Haskell Weekly is a free email newsletter about the Haskell programming language. Each issue features several hand-picked links to interesting content about Haskell from around the web.")
    , ("title", "Haskell Weekly")
    , ("url", "")
    ])

renderIssue :: Monad m => String -> String -> Context -> Issue -> m String
renderIssue baseTemplate issueTemplate context issue = do
  let title = issueTitle issue
  body <- renderTemplate issueTemplate (context ++
    [ ("body", issueContents issue)
    , ("date", prettyDay (issueDay issue))
    , ("title", title)
    ])
  renderTemplate baseTemplate (context ++
    [ ("body", body)
    , ("summary", "Issue " ++ show (issueNumber issue) ++ " of Haskell Weekly.")
    , ("title", title ++ " :: Haskell Weekly")
    , ("url", issueUrl issue)
    ])

renderRss :: Monad m => String -> String -> Context -> [Issue] -> m String
renderRss template itemTemplate context issues = do
  items <- mapM (renderRssItem itemTemplate context) issues
  renderTemplate template (("items", concat items) : context)

renderRssItem :: Monad m => String -> Context -> Issue -> m String
renderRssItem template context issue =
  renderTemplate template (context ++
    [ ("description", escapeHtml (issueContents issue))
    , ("pubDate", rfcDay (issueDay issue))
    , ("title", issueTitle issue)
    , ("url", issueUrl issue)
    ])

renderSnippet :: Monad m => String -> Context -> Issue -> m String
renderSnippet template context issue =
  renderTemplate template (context ++
    [ ("date", prettyDay (issueDay issue))
    , ("title", issueTitle issue)
    , ("url", issueUrl issue)
    ])

renderTemplate :: Monad m => String -> Context -> m String
renderTemplate template context = do
  let separator = '$'
  case break (== separator) template of
    (left, "") -> pure left
    (left, _ : middle) ->
      case break (== separator) middle of
        (_, "") -> fail "unterminated replacement"
        (key, _ : right) ->
          case lookup key context of
            Nothing -> fail ("unknown key: " ++ show key)
            Just value -> do
              rest <- renderTemplate right context
              pure (concat [left, value, rest])

-- Generic helpers

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f =
  f x

copyFileAt :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFileAt input output path =
  Directory.copyFile
    (FilePath.joinPath (input : path))
    (FilePath.joinPath (output : path))

createDirectoryAt :: [FilePath] -> IO ()
createDirectoryAt path =
  Directory.createDirectoryIfMissing True (FilePath.joinPath path)

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f =
  mapM_ f xs

hasExtension :: String -> FilePath -> Bool
hasExtension extension file =
  FilePath.takeExtension file == '.' : extension

listDirectoryAt :: [FilePath] -> IO [FilePath]
listDirectoryAt path =
  Directory.listDirectory (FilePath.joinPath path)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f l =
  case l of
    [] -> []
    h : t -> case f h of
      Nothing -> mapMaybe f t
      Just x -> x : mapMaybe f t

readFileAt :: [FilePath] -> IO String
readFileAt path = do
  handle <- IO.openFile (FilePath.joinPath path) IO.ReadMode
  IO.hSetEncoding handle IO.utf8
  IO.hGetContents handle

replace :: Char -> String -> String -> String
replace old new s =
  foldr (\ c t -> if c == old then new ++ t else c : t) "" s

safeHead :: [a] -> Maybe a
safeHead l =
  case l of
    [] -> Nothing
    h : _ -> Just h

safeRead :: Read a => String -> Maybe a
safeRead s =
  case readsPrec 0 s of
    [(x, "")] -> Just x
    _ -> Nothing

withDefault :: a -> Maybe a -> a
withDefault d m =
  case m of
    Nothing -> d
    Just x -> x

writeFileAt :: [FilePath] -> String -> IO ()
writeFileAt path contents = do
  handle <- IO.openFile (FilePath.joinPath path) IO.WriteMode
  IO.hSetEncoding handle IO.utf8
  IO.hPutStr handle contents
  IO.hFlush handle
