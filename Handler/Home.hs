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
    (uid, _) <- getsertUser Nothing
    pid <- runDB . insert $ Poll uid title desc cols vals now
    setMessageI $ MsgYouCreated title
    redirect $ SeePollR pid


getSeePollR :: PollId -> Handler Html
getSeePollR pid = do
    mUserId <- maybeAuthId
    poll <- runDB $ get404 pid
    results <- runDB $ selectList [ResultPoll ==. pid] [Asc ResultNickname]
    let answers = transpose $ map (resultAnswers . entityVal) results
        bests = map mostFrequent answers
        isRowMine p = maybe False (resultOwner p ==) mUserId
    (newRowForm, _) <- generateFormPost $ rowForm $ Entity pid poll
    defaultLayout $(widgetFile "see_poll")


postAjaxResultR :: PollId -> Handler TypedContent
postAjaxResultR pid = do
    poll <- runDB $ get404 pid
    ((res, _), _) <- runFormPost $ rowForm $ Entity pid poll
    case res of
        FormSuccess (nickname, values) -> do
            now <- liftIO getNow
            (uid, token) <- getsertUser (Just nickname)
            rid <- runDB . insert $ Result pid uid nickname values now
            return . toTypedContent . object $
                [ "pid"       .= pid
                , "rid"       .= rid
                , "nickname"  .= nickname
                , "values"    .= values
                , "uid"       .= uid
                , "token"     .= token
                ]
        _ ->
            invalidArgs ["Form probably incomplete"]


getsertUser :: Maybe Text -> Handler (UserId, Token)
getsertUser mNickname = do
    mUser <- maybeAuth
    case mUser of
         Just (Entity uid u) ->
             return (uid, userToken u)
         Nothing -> do
             token <- getYesod >>= liftIO . randomToken
             uid <- runDB . insert $ User token mNickname
             setCreds False $ tokenCreds token
             return (uid, token)


pollForm :: FormInput Handler (Text, Maybe Text, [Text], [Text])
pollForm = (\x y z u -> (x, y, z, u))
    <$> ireq textField "title"
    <*> iopt textField "desc"
    <*> ireq multiNonEmptyField "columns"
    <*> ireq multiNonEmptyField "values"


rowForm :: Entity Poll -> Html -> MForm Handler (FormResult (Text, [Text]), Widget)
rowForm (Entity pid poll) extra = do
    mr <- getMessageRender

    (_, pidView) <- mreq hiddenField "" (Just pid)

    let nikFset = fieldSettingsAttrs $
            [ ("class", "nickname form-control auto-size")
            , ("placeholder", mr $ SomeMessage MsgYourName) ]
    (nikRes, nikView) <- mreq textField nikFset Nothing

    let selectSize = length $ pollColumns poll
        answers = pollAnswers poll
        ansFset = fieldSettingsAttrs $
            [ ("size", textShow selectSize)
            , ("class", "cells form-control auto-size") ]
    (ansRess, ansViews) <- fmap unzip $ forM (pollColumns poll) $ \_ ->
        mreq (selectFieldList $ map (id &&& id) answers) ansFset Nothing

    let res = (,) <$> nikRes <*> sequenceA ansRess

    let widget = [whamlet|
      <td .nickname>
        #{extra}
        ^{fvInput pidView}
        ^{fvInput nikView}
      $forall v <- ansViews
        <td .cells>
          ^{fvInput v}
    |]

    return (res, widget)


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

fieldSettingsAttrs :: [(Text, Text)] -> FieldSettings site
fieldSettingsAttrs attrs = (fieldSettingsLabel emptyLabel) { fsAttrs = attrs }
    where emptyLabel = "" :: Text
