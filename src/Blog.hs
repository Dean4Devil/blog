module Blog
    ( renderIndex
    , renderPost
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

renderIndex :: FilePath -> Action ()
renderIndex path = do
    infiles <- getDirectoryFiles "" ["posts/*.md"]
    let outfiles = map (-<.> ".html") infiles
    posts <- mapM readPost' (zip infiles outfiles)
    writeHtmlFile path $ indexTemplate posts
  where
    readPost' (a, b) = readPost a b

renderPost :: FilePath -> FilePath -> Action ()
renderPost src out = do
    post <- readPost src out
    writeHtmlFile out $ postTemplate post
