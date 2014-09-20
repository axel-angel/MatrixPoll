{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time.Clock (getCurrentTime, utctDay)


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    (title, desc, cols, vals) <- runInputPost pollForm
    liftIO $ print (title, desc, cols, vals)
    now <- liftIO $ getCurrentTime >>= return . utctDay
    pid <- runDB . insert $ Poll Nothing title desc cols vals now
    setMessageI $ MsgYouCreated title
    redirect $ SeePollR pid


getSeePollR :: PollId -> Handler Html
getSeePollR pid = do
    poll <- runDB $ get404 pid
    defaultLayout $(widgetFile "see_poll")


pollForm :: FormInput Handler (Text, Maybe Text, [Text], [Text])
pollForm = (\x y z u -> (x, y, z, u))
    <$> ireq textField "title"
    <*> iopt textField "desc"
    <*> ireq multiNonEmptyField "columns"
    <*> ireq multiNonEmptyField "values"


multiNonEmptyField :: Field Handler [Text]
multiNonEmptyField = checkBool (not . null) MsgFieldRequireNonEmpty multiField

multiField :: Field Handler [Text]
multiField = Field
    { fieldParse = \xs _ -> return . Right . Just . filter ((/=) "") $ xs
    , fieldView = undefined
    , fieldEnctype = UrlEncoded
    }
