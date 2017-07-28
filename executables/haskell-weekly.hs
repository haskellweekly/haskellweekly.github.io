import Data.Function ((&))

import qualified CMark
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Read as Read

main :: IO ()
main = do
  let input = "content"
  baseTemplate <- readFile (FilePath.joinPath [input, "templates", "base.html"])
  form <- readFile (FilePath.joinPath [input, "includes", "form.html"])
  issueTemplate <- readFile (FilePath.joinPath [input, "templates", "issue.html"])
  logo <- readFile (FilePath.joinPath [input, "includes", "logo.svg"])
  script <- readFile (FilePath.joinPath [input, "includes", "script.js"])
  snippet <- readFile (FilePath.joinPath [input, "templates", "snippet.html"])
  style <- readFile (FilePath.joinPath [input, "includes", "style.css"])

  issueFiles <- Directory.listDirectory (FilePath.combine input "issues")
  issueContents <- issueFiles
    & filter (\ file -> FilePath.takeExtension file == ".markdown")
    & map FilePath.takeBaseName
    & Maybe.mapMaybe Read.readMaybe
    & map (\ number -> number :: Word)
    & List.sortBy (Ord.comparing Ord.Down)
    & mapM (\ number -> do
      contents <- readFile (FilePath.joinPath [input, "issues", FilePath.addExtension (show number) "markdown"])
      pure (number, contents))
  let issues = issueContents
        & Maybe.mapMaybe (\ (number, contents) -> case lines contents of
          first : rest -> pure (number, first, unlines rest)
          _ -> fail "missing first line")
        & Maybe.mapMaybe (\ (number, first, contents) -> case words first of
          ["<!--", date, "-->"] -> pure (number, date, contents)
          _ -> fail "invalid first line")
        & Maybe.mapMaybe (\ (number, date, contents) -> case Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" date of
          Just day -> pure (number, (day :: Time.Day), contents)
          _ -> fail "invalid date")
        & map (\ (number, day, contents) ->
          ( number
          , day
          , contents & Text.pack & CMark.commonmarkToHtml [] & Text.unpack
          ))

  let output = "_site"
  Directory.removePathForcibly output
  Directory.createDirectoryIfMissing True output

  Directory.createDirectoryIfMissing True (FilePath.combine output "images")
  Directory.copyFile (FilePath.joinPath [input, "images", "favicon.ico"]) (FilePath.joinPath [output, "images", "favicon.ico"])
  Directory.copyFile (FilePath.joinPath [input, "images", "twitter-card.png"]) (FilePath.joinPath [output, "images", "twitter-card.png"])

  Directory.createDirectoryIfMissing True (FilePath.combine output "issues")
  Monad.forM_ issues (\ (number, day, issue) -> do
    body <- render
      [ ("body", issue)
      , ("date", Time.formatTime Time.defaultTimeLocale "%B %e %Y" day)
      , ("form", form)
      , ("title", "Issue " ++ show number)
      ]
      issueTemplate
    contents <- render
      [ ("body", body)
      , ("logo", logo)
      , ("script", script)
      , ("style", style)
      , ("summary", "Issue " ++ show number ++ " of Haskell Weekly.")
      , ("title", "Issue " ++ show number ++ " :: Haskell Weekly")
      , ("url", "/issues/" ++ show number ++ ".html")
      ]
      baseTemplate
    writeFile (FilePath.joinPath [output, "issues", FilePath.addExtension (show number) "html"]) contents)

  do
    index <- readFile (FilePath.joinPath [input, "pages", "index.html"])
    snippets <- Monad.forM issues (\ (number, day, _) -> render
      [ ("date", Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" day)
      , ("title", "Issue " ++ show number)
      , ("url", "/issues/" ++ show number ++ ".html")
      ]
      snippet)
    body <- render
      [ ("form", form)
      , ("issues", concat snippets)
      ] index
    contents <- render
      [ ("body", body)
      , ("form", form)
      , ("logo", logo)
      , ("script", script)
      , ("style", style)
      , ("summary", "Haskell Weekly is a free email newsletter about the Haskell programming language. Each issue features several hand-picked links to interesting content about Haskell from around the web.")
      , ("title", "Haskell Weekly")
      , ("url", "/index.html")
      ]
      baseTemplate
    writeFile (FilePath.combine output "index.html") contents

render :: Monad m => [(String, String)] -> String -> m String
render values template = do
  let separator = '$'
  case break (== separator) template of
    (left, "") -> pure left
    (left, _ : middle) ->
      case break (== separator) middle of
        (_, "") -> fail "unterminated replacement"
        (key, _ : right) ->
          case lookup key values of
            Nothing -> fail ("unknown key: " ++ show key)
            Just value -> do
              rest <- render values right
              pure (concat [left, value, rest])
