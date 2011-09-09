{-# LANGUAGE OverloadedStrings #-}

module Links where

import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T

links :: T.Text -> [T.Text]
links = filter (not . T.null) . map (fromAttrib "href") . filter (\t -> fromAttrib "rel" t /= "nofollow") . filter (isTagOpenName "a") . canonicalizeTags . parseTags

