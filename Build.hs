module Main where

import Prelude hiding ((*>))
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Data.Text.Lazy (unpack)

import Text.Lucius (renderCss)
import Text.Pandoc
import Text.Pandoc.Error
import Text.Blaze (string)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Text.Atom.Feed

import Paranoidlabs.Templates

build :: FilePath -> FilePath
build = (</>) "out"

markdownToHtml :: String -> Html
markdownToHtml = writeHtml def . handleError . readMarkdown def

copy :: FilePath -> Rules ()
copy pattern = build pattern *> \out -> copyFile' (dropDirectory1 out) out

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = "out" } $ do

    -- We want two static files:
    want $ map build ["index.html", "css/style.css"]
    -- But we also want every post compiled independently.
    action $ do
        posts <- getDirectoryFiles "" ["posts/*.md"]
        let posthtml = map ((build) . (-<.> ".html")) posts
        need posthtml

    phony "clean" $ removeFilesAfter "out" ["//*"]

    getPost <- newCache $ \path -> do
        md <- markdownToHtml <$> readFile' path
        return md

    build "index.html" *> \out -> do
        need ["template/index.hamlet"]
        posts <- getDirectoryFiles "" ["posts/*.md"]
        postmd <- mapM readFile' posts
        let postpd = map (handleError . (readMarkdown def)) postmd
        let poststr = map (take 200 . writePlain def) postpd
        writeFile' out $ renderHtml $ index poststr

    build "posts/*.html" *> \out -> do
        let src = (-<.> ".md") . dropDirectory1 $ out
        need [src, "template/post.hamlet"]
        posttext <- getPost src
        writeFile' out $ renderHtml $ post posttext

    build "css/style.css" *> \out -> do
        alwaysRerun -- TODO Improve this
        let src = (-<.> ".scss") . ("style" </>) . dropDirectory1 . dropDirectory1 $ out
        partials <- getDirectoryFiles "" ["style/*.scss"]
        need $ [src] ++ partials
        Stdout css <- command [] "sassc" [src, "-I", "style"]
        writeFile' out css
