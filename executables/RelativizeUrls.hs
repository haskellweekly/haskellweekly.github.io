-- | This entire module exists because Hakyll's @relativizeUrls@ function does
-- not relativize URLs in the "content" attributes of "meta" tags. The new
-- function provided by this module does.
module RelativizeUrls
  ( relativizeUrls
  ) where

import qualified Hakyll
import qualified Text.HTML.TagSoup as TagSoup

relativizeUrls :: Hakyll.Item String -> Hakyll.Compiler (Hakyll.Item String)
relativizeUrls item = do
  let identifier = Hakyll.itemIdentifier item
  maybeRoute <- Hakyll.getRoute identifier
  let newItem =
        case maybeRoute of
          Nothing -> item
          Just route -> do
            let root = Hakyll.toSiteRoot route
            let relativize = relativizeUrlsWith root
            fmap relativize item
  pure newItem

relativizeUrlsWith :: String -> String -> String
relativizeUrlsWith root html =
  let relativizeUrl = relativizeUrlWith root
  in withUrls relativizeUrl html

relativizeUrlWith :: String -> String -> String
relativizeUrlWith root url =
  if shouldRelativizeUrl url
    then root ++ url
    else url

shouldRelativizeUrl :: String -> Bool
shouldRelativizeUrl url =
  case url of
    '/':'/':_ -> False
    '/':_ -> True
    _ -> False

withUrls :: (String -> String) -> String -> String
withUrls modify html =
  let relativizeTag = relativizeTagWith modify
  in Hakyll.withTags relativizeTag html

relativizeTagWith :: (String -> String)
                  -> TagSoup.Tag String
                  -> TagSoup.Tag String
relativizeTagWith modify tag =
  case tag of
    TagSoup.TagOpen name attributes ->
      let relativizeAttribute = relativizeAttributeWith modify name
          newAttributes = map relativizeAttribute attributes
      in TagSoup.TagOpen name newAttributes
    _ -> tag

relativizeAttributeWith
  :: (String -> String)
  -> String
  -> TagSoup.Attribute String
  -> TagSoup.Attribute String
relativizeAttributeWith modify tagName (name, value) =
  let newValue =
        if shouldRelativizeAttribute tagName name
          then modify value
          else value
  in (name, newValue)

shouldRelativizeAttribute :: String -> String -> Bool
shouldRelativizeAttribute tagName name =
  case (tagName, name) of
    ("meta", "content") -> True
    (_, "data") -> True
    (_, "href") -> True
    (_, "poster") -> True
    (_, "src") -> True
    _ -> False
