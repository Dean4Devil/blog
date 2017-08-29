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

needPosts = do
    posts <- getDirectoryFiles "" ["posts/*.md"]
    let posthtml = map (build . (-<.> ".html")) posts
    need posthtml

main :: IO ()
main =
    shakeArgs shakeOptions {shakeFiles = "out"} $
    -- We want two static files:
     do
        want $ map build ["index.html", "css/style.css"]
    -- But we also want every post compiled independently.
        action needPosts
        phony "clean" $ removeFilesAfter outdir ["//*"]
        phony "serve" $ do
            need $ map build ["index.html", "css/style.css"]
            needPosts
            command_ [Cwd outdir] "python" ["-m", "http.server"]
        build "index.html" *> \out -> do
            need ["template/index.hamlet"]
            renderIndex out
        build "posts/*.html" *> \out -> do
            let src = (-<.> ".md") . dropDirectory1 $ out
            need [src, "template/post.hamlet"]
            renderPost src out
        build "css/style.css" *> \out -> do
            alwaysRerun -- FIXME: Improve this
            let src = (-<.> ".scss") . ("style" </>) . dropDirectory1 . dropDirectory1 $ out
            partials <- getDirectoryFiles "" ["style/*.scss"]
            need $ src : partials
            Stdout css <- command [] "sassc" [src, "-I", "style"]
            writeFile' out css
