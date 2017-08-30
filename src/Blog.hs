module Blog
    ( generateList
    , generatePost
    ) where

import Blog.Post
import Blog.Templates
import Blog.Write

import Development.Shake
       (Action, getDirectoryFiles, liftIO, readFile', writeFile')
import Development.Shake.FilePath

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc
import Text.Pandoc.Walk

import Data.Map.Lazy (fromList)

outdir = "out"

build :: FilePath -> FilePath
build = (</>) outdir

readMarkdown' :: String -> Pandoc
readMarkdown' = handleError . readMarkdown def

writeHtmlFile :: FilePath -> Html -> Action ()
writeHtmlFile path html = writeFile' path (renderHtml html)

generateList :: FilePath -> Action ()
generateList path = do
    infiles <- getDirectoryFiles "" ["posts/*md"]
    let outfiles = map (-<.> ".html") infiles
    posts <- mapM readPost' (zip infiles outfiles)
    writeHtmlFile path $ listView posts
  where
    readPost' = uncurry readPost

generatePost :: FilePath -> FilePath -> Action ()
generatePost src out = do
    post <- readPost src out
    writeHtmlFile out $ detailView post
