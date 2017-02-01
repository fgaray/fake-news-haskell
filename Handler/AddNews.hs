
-- | Common handler functions.
module Handler.AddNews where

import Import


getAddNewsR :: Handler Html
getAddNewsR = do
    defaultLayout $ do
    $(widgetFile "AddNews")

