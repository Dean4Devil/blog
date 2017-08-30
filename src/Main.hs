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

needPosts = need <$> allPosts

-- Define all build targets here
buildTargets :: Rules ()
buildTargets = action $ do
    need ["out/index.html", "out/css/style.css"]
    need <$> allPosts

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "out"} $ do
    buildTargets

    build "index.html" *> \out -> do
        need ["template/index.hamlet"]
        generateList out
    "out/posts/*.html" %> \out -> do
        let src = (-<.> ".md") . dropDirectory1 $ out
        need [src, "template/post.hamlet"]
        generatePost src out
    build "css/style.css" *> \out -> do
        alwaysRerun -- FIXME: Improve this
        let src = (-<.> ".scss") . ("style" </>) . dropDirectory1 . dropDirectory1 $ out
        partials <- getDirectoryFiles "" ["style/*.scss"]
        need $ src : partials
        Stdout css <- command [] "sassc" [src, "-I", "style"]
        writeFile' out css

    "serve" ~> do
        need $ map build ["index.html", "css/style.css"]
        _ <- needPosts
        command_ [Cwd outdir] "python" ["-m", "http.server"]
    "clean" ~> removeFilesAfter outdir ["//*"]
