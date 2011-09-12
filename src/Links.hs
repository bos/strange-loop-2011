module Links where

import Data.Maybe
import Network.URI
import Text.HTML.TagSoup

links :: String -> String -> [String]
links url = catMaybes .
            map (canonicalizeLink url) .
            filter (not . null) .
            map (fromAttrib "href") .
            filter (\t -> fromAttrib "rel" t /= "nofollow") .
            filter (isTagOpenName "a") .
            canonicalizeTags .
            parseTags

canonicalizeLink :: String -> String -> Maybe String
canonicalizeLink referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  n <- p `nonStrictRelativeTo` r
  return (uriToString id n "")
