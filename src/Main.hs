module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Prelude hiding ((*>))

import Data.Map.Lazy (fromList)
import Data.Text.Lazy (unpack)

import Text.Blaze (string)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Lucius (renderCss)
import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk

import Text.Atom.Feed

import Blog
import Blog.Templates

outdir = "out"

build :: FilePath -> FilePath
build = (</>) outdir

markdownToHtml :: String -> Html
markdownToHtml = writeHtml def . handleError . readMarkdown def

allPosts :: Action [FilePath]
allPosts = do
    sources <- getDirectoryFiles "" ["posts/*md"]
    return $ map (build . (-<.> ".html")) sources

needAllPosts :: Action ()
needAllPosts = need =<< allPosts

-- Define all build targets here
buildTargets :: Rules ()
buildTargets = action $ do
    need ["out/index.html", "out/styles/default.css"]
    needAllPosts

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "out"} $ do
    buildTargets

    -- Rules:
    "out/index.html" %> \out -> do
        needAllPosts
        generateList out
    "out/posts/*.html" %> \out -> do
        let src = (-<.> ".md") . dropDirectory1 $ out
        need [src]
        generatePost src out
    "out/styles/default.css" %> \out -> do
        alwaysRerun -- FIXME: Improve this
        let src = (-<.> ".scss") . ("style" </>) . dropDirectory1 . dropDirectory1 $ out
        partials <- getDirectoryFiles "" ["style/*.scss"]
        need $ src : partials
        Stdout css <- command [] "sassc" [src, "-I", "style"]
        writeFile' out css

    -- Phony rules
    "serve" ~> do
        need $ map build ["index.html", "css/style.css"]
        _ <- needAllPosts
        command_ [Cwd outdir] "python" ["-m", "http.server"]
    "clean" ~> removeFilesAfter outdir ["//*"]
