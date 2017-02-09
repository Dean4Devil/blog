{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Paranoidlabs.Templates where

import Text.Blaze.Html (Html)
import Text.Hamlet

data Urls = Test
render :: Render Urls
render Test _ = "/test"

postpage :: Html -> Html
postpage text =
    $(shamletFile "template/default.hamlet")
  where
    title = "Uh.. A post?"::String
    content = post text

index :: [String] -> Html
index posts =
    $(shamletFile "template/default.hamlet")
  where
    title = "Uh.. A blog?"::String
    content = $(shamletFile "template/index.hamlet")

navigation :: Html
navigation =
    $(shamletFile "template/navigation.hamlet")

calendar :: Html
calendar =
    $(shamletFile "template/calendar.hamlet")

post :: Html -> Html
post text =
    $(shamletFile "template/post.hamlet")
