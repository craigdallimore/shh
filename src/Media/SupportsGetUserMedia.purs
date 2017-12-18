module Media.SupportsGetUserMedia where

import DOM.HTML.Types (Navigator)

foreign import supportsGetUserMedia :: Navigator -> Boolean
