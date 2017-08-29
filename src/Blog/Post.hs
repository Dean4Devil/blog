{-# LANGUAGE OverloadedStrings #-}

module Blog.Post
    ( extractTitle
    , extractAbstract
    , Post
    , readPost
    , postUrl
    , postDoc
    ) where

import Development.Shake
import Development.Shake.FilePath

import Data.Map.Lazy (fromList)
import Data.Text

import Text.Blaze.Html (Html)
import Text.Pandoc
import Text.Pandoc.Walk

data Post = Post
    { doc :: Pandoc
    , url :: String
    }

readMarkdown' :: String -> Pandoc
readMarkdown' = handleError . readMarkdown def

readPost :: FilePath -> FilePath -> Action Post
readPost src out = do
    let file = readFile' src
    let post = fmap readMarkdown' file
    fmap (\p -> Post p (dropDirectory1 out)) post

postUrl :: Post -> String
postUrl (Post _ url) = url

postDoc :: Post -> Pandoc
postDoc (Post doc _) = doc

showInline :: [Inline] -> String
showInline i = writeHtmlString def (Pandoc Meta {unMeta = fromList []} [Plain i])

extractTitle :: Post -> String
extractTitle (Post (Pandoc meta _) _) = query (showInline . docTitle) meta

docAbstract :: Meta -> [Inline]
docAbstract meta =
    case lookupMeta "abstract" meta of
        Just (MetaString s) -> [Str s]
        Just (MetaInlines ils) -> ils
        Just (MetaBlocks [Plain ils]) -> ils
        Just (MetaBlocks [Para ils]) -> ils
        _ -> []

extractAbstract :: Post -> String
extractAbstract (Post (Pandoc meta _) _) = query (showInline . docAbstract) meta
