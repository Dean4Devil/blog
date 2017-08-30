{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Blog.Templates
    ( listView
    , detailView
    ) where

import Blog.Post
import Blog.Write

import Data.Text

import Text.Blaze.Html (Html)
import Text.Hamlet

import Text.Pandoc

data Urls
    = CSS
    | Home
    | Github
    | Twitter
    | PostLink Post

render :: Render Urls
render CSS _ = "/css/style.css"
render Home _ = "/"
render Github _ = "https://github.com/dequbed"
render Twitter _ = "https://twitter.com/dequbed"
render (PostLink post) _ = pack $ "/posts/" ++ postUrl post

navigation :: HtmlUrl Urls
navigation = $(hamletFile "template/navigation.hamlet")

calendar :: HtmlUrl Urls
calendar = $(hamletFile "template/calendar.hamlet")

listView :: [Post] -> Html
listView posts = $(hamletFile "template/default.hamlet") render
  where
    title = "Uh.. A blog?" :: String
    content = $(hamletFile "template/listview.hamlet")

detailView :: Post -> Html
detailView post = $(hamletFile "template/default.hamlet") render
  where
    title = extractTitle post
    doc = postDoc post
    text = writeHtml' doc
    content = $(hamletFile "template/detailview.hamlet")
