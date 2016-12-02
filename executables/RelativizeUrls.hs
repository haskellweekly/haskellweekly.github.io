module RelativizeUrls
  ( relativizeUrls
  ) where

import qualified Hakyll

relativizeUrls :: Hakyll.Item String -> Hakyll.Compiler (Hakyll.Item String)
relativizeUrls = Hakyll.relativizeUrls
