{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Data.List (sort, group, transpose)
import GHC.Exts (sortWith)


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    (title, desc, cols, vals) <- runInputPost pollForm
    liftIO $ print (title, desc, cols, vals)
    now <- liftIO getNow
    pid <- runDB . insert $ Poll Nothing title desc cols vals now
    setMessageI $ MsgYouCreated title
    redirect $ SeePollR pid


getSeePollR :: PollId -> Handler Html
getSeePollR pid = do
    poll <- runDB $ get404 pid
    results <- runDB $ selectList [ResultPoll ==. pid] [Asc ResultNickname]
    let answers = transpose $ map (resultAnswers . entityVal) results
    let bests = map mostFrequent answers
    defaultLayout $(widgetFile "see_poll")


postAjaxResultR :: Handler TypedContent
postAjaxResultR = do
    res <- runInputPostResult rowForm
    case res of
        FormSuccess (pid, nickname, values) -> do
            now <- liftIO getNow
            rid <- runDB . insert $ Result pid Nothing nickname values now
            (uid, token) <- getsertUser nickname
            setCreds False $ tokenCreds token
            return . toTypedContent . object $
                [ "success"   .= True
                , "pid"       .= pid
                , "rid"       .= rid
                , "nickname"  .= nickname
                , "values"    .= values
                , "uid"       .= uid
                , "token"     .= token
                ]
        _ ->
            return . toTypedContent . object $ [ "success" .= False ]


getsertUser :: Text -> Handler (UserId, Token)
getsertUser nickname = do
    mUser <- maybeAuth
    case mUser of
         Just (Entity uid u) ->
             return (uid, userToken u)
         Nothing -> do
             master <- getYesod
             token <- liftIO $ randomToken master
             uid <- runDB . insert $ User token nickname
             return (uid, token)


pollForm :: FormInput Handler (Text, Maybe Text, [Text], [Text])
pollForm = (\x y z u -> (x, y, z, u))
    <$> ireq textField "title"
    <*> iopt textField "desc"
    <*> ireq multiNonEmptyField "columns"
    <*> ireq multiNonEmptyField "values"


rowForm :: FormInput Handler (PollId, Text, [Text])
rowForm = (\x y z -> (x, y, z))
    <$> ireq (selectField pollKeyField) "pid"
    <*> ireq textField "nickname"
    <*> ireq multiNonEmptyField "values"


pollKeyField :: Handler (OptionList (Key Poll))
pollKeyField = optionsPersistKey noFilter [] (const msg)
    where msg = toMessage ("" :: Text)
          noFilter = []::[Filter Poll]


multiNonEmptyField :: Field Handler [Text]
multiNonEmptyField = checkBool (not . null) MsgFieldRequireNonEmpty multiField

multiField :: Field Handler [Text]
multiField = Field
    { fieldParse = \xs _ -> return . Right . Just . filter ((/=) "") $ xs
    , fieldView = undefined
    , fieldEnctype = UrlEncoded
    }

getNow :: IO Day
getNow = getCurrentTime >>= return . utctDay


mostFrequent :: (Eq a, Ord a) => [a] -> Maybe (a, Int)
mostFrequent = safeHead . reverse . sortWith (\(_,c) -> c) . countOccurences
    where safeHead (x:_) = Just x
          safeHead _ = Nothing


countOccurences :: (Eq a, Ord a) => [a] -> [(a, Int)]
countOccurences = map (\xs@(x:_) -> (x, length xs)) . group . sort
