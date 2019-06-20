#!/usr/bin/env stack
-- stack --resolver lts-13.20 script
{-# OPTIONS_GHC -Weverything -Wno-implicit-prelude -Wno-unsafe #-}

-- | This script checks all of the links in every issue to make sure that they
-- still exist.
module Main
  ( main
  )
where

import qualified Control.Exception as Exception
import qualified CMark as Markdown
import qualified Data.ByteString as Bytes
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified System.Directory as Directory
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified Text.Printf as Printf

main :: IO ()
main = do
  let directory = Path.combine "content" "issues"
  manager <- Client.newTlsManager

  issues <- getIssues directory
  Printf.printf
    "Found %d issue%s.\n"
    (length issues)
    (if length issues == 1 then "" else "s")

  links <- getLinks directory issues
  Printf.printf
    "Found %d link%s.\n"
    (Set.size links)
    (if Set.size links == 1 then "" else "s")

  mapM_ (checkLink manager) links

getIssues :: FilePath -> IO [FilePath]
getIssues directory = do
  files <- Directory.listDirectory directory
  pure (filter isMarkdown files)

isMarkdown :: FilePath -> Bool
isMarkdown file = hasExtension "markdown" file

hasExtension :: String -> FilePath -> Bool
hasExtension extension file =
  Path.takeExtension file == Path.extSeparator : extension

getLinks :: FilePath -> [FilePath] -> IO (Set.Set Uri.URI)
getLinks directory issues = do
  uris <- extractIssuesUris directory issues
  let httpUris = Set.filter isHttp uris
  pure httpUris

extractIssuesUris :: FilePath -> [FilePath] -> IO (Set.Set Uri.URI)
extractIssuesUris directory issues = do
  urls <- extractIssuesUrls directory issues
  let uris = Maybe.mapMaybe parseUri urls
  pure (Set.fromList uris)

extractIssuesUrls :: FilePath -> [FilePath] -> IO [Markdown.Url]
extractIssuesUrls directory issues = do
  urls <- mapM (extractIssueUrls directory) issues
  pure (concat urls)

extractIssueUrls :: FilePath -> FilePath -> IO [Markdown.Url]
extractIssueUrls directory issue = do
  let file = Path.combine directory issue
  text <- Text.readFile file
  let node = Markdown.commonmarkToNode [] text
  pure (extractNodeUrls node)

extractNodeUrls :: Markdown.Node -> [Markdown.Url]
extractNodeUrls (Markdown.Node _ nodeType nodes) =
  let links = concatMap extractNodeUrls nodes
  in
    case extractNodeTypeUrl nodeType of
      Nothing -> links
      Just link -> link : links

extractNodeTypeUrl :: Markdown.NodeType -> Maybe Markdown.Url
extractNodeTypeUrl nodeType = case nodeType of
  Markdown.LINK url _ -> Just url
  _ -> Nothing

parseUri :: Markdown.Url -> Maybe Uri.URI
parseUri url = Uri.parseAbsoluteURI (Text.unpack url)

isHttp :: Uri.URI -> Bool
isHttp uri = hasScheme "http" uri || hasScheme "https" uri

hasScheme :: String -> Uri.URI -> Bool
hasScheme scheme uri = safeInit (Uri.uriScheme uri) == Just scheme

safeInit :: [a] -> Maybe [a]
safeInit xs = case xs of
  [] -> Nothing
  _ -> Just (init xs)

checkLink :: Client.Manager -> Uri.URI -> IO ()
checkLink manager uri = do
  let url = displayUri uri
  request <- Client.parseRequest url
  Exception.catch
    (do
      response <- Client.httpNoBody (withUserAgent request) manager
      case Http.statusCode (Client.responseStatus response) of
        200 -> Printf.printf "- 200 %s\n" url
        code -> IO.hPutStrLn IO.stderr (Printf.printf "- %d %s" code url)
    )
    (\exception -> case exception of
      Client.HttpExceptionRequest _ x ->
        IO.hPutStrLn IO.stderr (Printf.printf "- 001 %s %s" url (show x))
      Client.InvalidUrlException _ x ->
        IO.hPutStrLn IO.stderr (Printf.printf "- 002 %s %s" url x)
    )

displayUri :: Uri.URI -> String
displayUri uri = Uri.uriToString id uri ""

withUserAgent :: Client.Request -> Client.Request
withUserAgent request =
  request { Client.requestHeaders = [(Http.hUserAgent, userAgent)] }

userAgent :: Bytes.ByteString
userAgent = Text.encodeUtf8 (Text.pack "haskell-weekly")
