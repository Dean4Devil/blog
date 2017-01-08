{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Paranoidlabs.Templates where

import Text.Blaze.Html (Html)
import Text.Hamlet

data Urls = Test
render :: Render Urls
render Test _ = "/test"

postLayout :: Html -> Html
postLayout content =
    $(shamletFile "template/post.hamlet")
  where
      sidebar = $(shamletFile "template/sidebar.hamlet")

indexLayout :: Html -> Html
indexLayout content =
    $(shamletFile "template/index.hamlet")
  where
    sidebar = $(shamletFile "template/sidebar.hamlet")
