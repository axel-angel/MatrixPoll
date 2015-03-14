{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Data.List (sort, group, transpose)
import GHC.Exts (sortWith)
import Safe (atMay)
import Control.Monad (join)
import System.Random (randomRIO)
import Data.Text (pack)


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    (title, desc, cols, vals') <- runInputPost pollForm
    let vals = if null vals' then ["Yes", "No", "Maybe"] else vals'
    now <- liftIO getNow
    (uid, _) <- getsertUser Nothing
    phash <- mkPollHash
    _pid <- runDB . insert $ Poll phash uid title desc cols vals now
    setMessageI $ MsgYouCreated title
    redirect $ SeePollR phash


getSeePollR :: Text -> Handler Html
getSeePollR phash = do
    mUserId <- maybeAuthId
    Entity pid poll <- runDB $ getBy404 $ UniquePoll phash
    results <- runDB $ selectList [ResultPoll ==. pid] [Asc ResultNickname]
    forms <- forM results $ \(Entity _ r) ->
        fmap fst $ generateFormPost $ rowForm (Entity pid poll) (Just r)
    let answers = transpose $ map (resultAnswers . entityVal) results
        bests = map mostFrequent answers
        isRowMine p = maybe False (resultOwner p ==) mUserId
    (newRowForm, _) <- generateFormPost $ rowForm (Entity pid poll) Nothing
    defaultLayout $(widgetFile "see_poll")


postAjaxAddR :: PollId -> Handler TypedContent
postAjaxAddR pid = do
    poll <- runDB $ get404 pid
    ((res, _), _) <- runFormPost $ rowForm (Entity pid poll) Nothing
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


postAjaxUpdateR :: PollId -> ResultId -> Handler TypedContent
postAjaxUpdateR pid rid = do
    uid <- requireAuthId
    poll <- runDB $ get404 pid
    result <- runDB $ get404 rid

    when (resultOwner result /= uid) $ do
        invalidArgs ["You don't own this row"]

    ((res, _), _) <- runFormPost $ rowForm (Entity pid poll) Nothing
    case res of
        FormSuccess (nickname, values) -> do
            now <- liftIO getNow
            let result' = result { resultNickname = nickname
                                 , resultAnswers = values
                                 , resultDate = now }
            runDB $ replace rid result'
            return . toTypedContent . object $ [ ]
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
    <*> ireq multiField "values"


rowForm :: Entity Poll -> Maybe Result -> Html -> MForm Handler (FormResult (Text, [Text]), Widget)
rowForm (Entity pid poll) mResult extra = do
    mr <- getMessageRender

    (_, pidView) <- mreq hiddenField "" (Just pid)

    let nikFset = fieldSettingsAttrs $
            [ ("class", "nickname form-control auto-size")
            , ("placeholder", mr $ SomeMessage MsgYourName) ]
    (nikRes, nikView) <- mreq textField nikFset (resultNickname <$> mResult)

    let selectSize = length $ pollAnswers poll
        answers = pollAnswers poll
        ansFset = fieldSettingsAttrs $
            [ ("size", textShow selectSize)
            , ("class", "cells form-control auto-size") ]
        mAnsAt i = join $ flip atMay i . resultAnswers <$> mResult
        columnCount = length $ pollColumns poll
    (ansRess, ansViews) <- fmap unzip $ forM [0..columnCount-1] $ \i ->
        mreq (selectFieldList $ map (id &&& id) answers) ansFset (mAnsAt i)

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


pollHashLen :: Int
pollHashLen = 4

pollChars :: [Char]
pollChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

pickElem :: [a] -> IO a
pickElem xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

mkPollHash :: Handler Text
mkPollHash = do
    suf <- liftIO $ forM [1 .. pollHashLen-1] $ const $ pickElem pollChars
    pack <$> appendCheck suf
    where
        appendCheck :: [Char] -> Handler [Char]
        appendCheck suffix = do
            hash' <- (: suffix) <$> (liftIO $ pickElem pollChars)
            mPoll <- runDB . getBy $ UniquePoll (pack hash')
            case mPoll of
                 Nothing -> return hash'
                 Just _ -> appendCheck hash'
