module Main where

import Prelude hiding ((*>))
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Text.Pandoc
import Text.Pandoc.Error
import Text.Blaze (string)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Text.Atom.Feed

import Paranoidlabs.Templates

build = (</>) "out"

markdownToHtml :: String -> Html
markdownToHtml = writeHtml def . handleError . readMarkdown def

copy :: FilePath -> Rules ()
copy pattern = build pattern *> \out -> copyFile' (dropDirectory1 out) out

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = "out" } $ do

    want $ ["out/index.html"]

    phony "clean" $ removeFilesAfter "out" ["//*"]

    getPost <- newCache $ \path -> do
        md <- markdownToHtml <$> readFile' path
        return md

    build "index.html" *> \out -> do
        posts <- getDirectoryFiles "" ["posts/*.md"]
        let postsrcs = map ((-<.> ".html") . build) $ posts
        need $ ["template/index.hamlet"] ++ postsrcs
        idx <- readFile' $ dropDirectory1 out
        writeFile' out $ renderHtml $ indexLayout $ (writeHtml def . handleError . readMarkdown def) idx

    build "posts/*.html" *> \out -> do
        let src = (-<.> ".md") . dropDirectory1 $ out
        need $ [src, "template/post.hamlet"]
        post <- getPost src
        writeFile' out $ renderHtml $ postLayout post
