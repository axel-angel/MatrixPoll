{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    (title, desc, cols, vals) <- runInputPost pollForm
    liftIO $ print (title, desc, cols, vals)
    defaultLayout $(widgetFile "homepage")


pollForm :: FormInput Handler (Text, Maybe Text, [Text], [Text])
pollForm = (\x y z u -> (x, y, z, u))
    <$> ireq textField "title"
    <*> iopt textField "desc"
    <*> ireq multiField "columns"
    <*> ireq multiField "values"


multiField :: Field Handler [Text]
multiField = Field
    { fieldParse = \xs _ -> return . Right . Just $ xs
    , fieldView = undefined
    , fieldEnctype = UrlEncoded
    }
