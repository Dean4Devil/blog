module Blog.Write
    ( writeHtml'
    ) where

import Text.Blaze.Html (Html)
import Text.Pandoc

own :: WriterOptions
own = def {writerHighlight = True}

writeHtml' :: Pandoc -> Html
writeHtml' = writeHtml own
