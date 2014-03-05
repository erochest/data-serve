{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Form
    ( dataViewForm
    , runDataViewForm
    ) where


import           Application
import           Control.Applicative
import           Control.Error
import qualified Data.Map                      as M
import           Data.Monoid
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Heist
import qualified Heist.Interpreted             as I
import           Model
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Text.Digestive
import           Text.Digestive.Heist
import qualified Text.XmlHtml                  as X


pageSize :: Int
pageSize = 25


dataViewForm :: Monad m => [T.Text] -> Form T.Text m DataView
dataViewForm countries =
        DataView
    <$> "country" .: choice countries' Nothing
    <*> "page"    .: stringRead "" (Just 0)
    where countries' = map (\c -> (c, c)) countries

runGetDataView :: (Monad m, MonadSnap m)
               => T.Text -> Form v m DataView
               -> m (View v, Maybe DataView)
runGetDataView prefix form = do
    ps <- getParams
    if M.null ps
        then (, Nothing) <$> getForm prefix form
        else postForm prefix form $ env ps

makeDataViewUrl :: View v -> T.Text -> T.Text -> Int -> T.Text
makeDataViewUrl view base country page =
    T.concat [base, "?", countryQ, "=", country, "&", pageQ, "=", tshow page]
    where countryQ = absoluteRef "country" view
          pageQ    = absoluteRef "page"    view

env :: Monad m => Params -> FormEncType -> m (Env m)
env ps _ = return ( return
                  . concat
                  . maybeToList
                  . fmap (map (TextInput . decodeUtf8))
                  . (`M.lookup` ps)
                  . encodeUtf8
                  . fromPath
                  )

runDataViewForm :: Handler App Postgres ()
runDataViewForm = do
    countries <- query_ "SELECT DISTINCT country\
                        \ FROM internet_users\
                        \ ORDER BY country";
    (view, result) <- runGetDataView "data"
                   .  dataViewForm
                   $  concat countries
    maybe (renderForm view) (renderDataView view) result

renderDataView :: View v -> DataView -> Handler App Postgres ()
renderDataView view DataView{..} = do
    -- Construct the URL for the pager
    baseUrl <- withRequest $ \r ->
        return . decodeUtf8 $ rqContextPath r <> rqPathInfo r
    country <-  fromMaybe "" . fmap decodeUtf8
            <$> (getParam . encodeUtf8 $ absoluteRef "country" view)
    let url = makeDataViewUrl view baseUrl country

    totalIUsers <-  maybe 0 fromOnly
                .   listToMaybe
                <$> query "SELECT COUNT(*) FROM internet_users WHERE country=?;"
                          (Only dataViewCountry)
    iusers :: [InternetUser] <- query "SELECT *\
                    \ FROM internet_users\
                    \ WHERE country=?\
                    \ ORDER BY id\
                    \ LIMIT ?\
                    \ OFFSET ?;"
                    (dataViewCountry, pageSize, dataViewPage * pageSize)
    renderWithSplices "iuser_table" $ do
        "iusers" ## renderInternetUsers iusers
        "pager"  ## pager url dataViewPage pageSize totalIUsers

renderInternetUser :: Monad m => InternetUser -> I.Splice m
renderInternetUser InternetUser{..} = do
    I.runChildrenWithText $ do
        "id"          ## tshow _internetUserId
        "country"     ## _internetUserCountry
        "countryCode" ## _internetUserCountryCode
        "year"        ## _internetUserYear
        "value"       ## maybe "" tshow _internetUserCount

tshow :: Show a => a -> T.Text
tshow = T.pack . show

renderInternetUsers :: Monad m => [InternetUser] -> I.Splice m
renderInternetUsers = I.mapSplices renderInternetUser

renderForm :: View T.Text -> Handler App Postgres ()
renderForm view =
    heistLocal (bindDigestiveSplices view) $ render "country_name_form"

pager :: Monad m => (Int -> T.Text) -> Int -> Int -> Int -> I.Splice m
pager mkUrl pageNo size totalItems = do
    return [ X.Element "ul" [("class", "pagination pull-right")] $
                       [ liFirst 0 pageNo (mkUrl 0)
                       , liPrev 0 (pred pageNo) $ mkUrl (pred pageNo)
                       ] ++
                       [ li pageNo n (mkUrl n) | n <- pages ] ++
                       [ liNext maxPage (succ pageNo) $ mkUrl (succ pageNo)
                       , liLast maxPage pageNo (mkUrl $ pred maxPage)
                       ]
           ]
    where maxPage' = totalItems `div` size
          maxPage  = if (totalItems `mod` size) > 0
                         then succ maxPage'
                         else maxPage'
          pages    = filter (< maxPage)
                   . take 5
                   $ filter (>= 0) [(pageNo - 2) .. ]

liFirst :: Int -> Int -> T.Text -> X.Node
liFirst first current url
    | first == current || first >= (current - 2) =
        X.Element "li" [("class", "disabled")] [spanTag arrows]
    | otherwise =
        X.Element "li" [] [a url arrows]
    where arrows = [X.TextNode "«"]

liPrev :: Int -> Int -> T.Text -> X.Node
liPrev first prev url
    | prev < first =
        X.Element "li" [("class", "disabled")] [spanTag arrows]
    | otherwise =
        X.Element "li" [] [a url arrows]
    where arrows = [X.TextNode "‹"]

liNext :: Int -> Int -> T.Text -> X.Node
liNext lastPage next url
    | lastPage <= next =
        X.Element "li" [("class", "disabled")] [spanTag arrows]
    | otherwise =
        X.Element "li" [] [a url arrows]
    where arrows = [X.TextNode "›"]

liLast :: Int -> Int -> T.Text -> X.Node
liLast maxPage current url
    | current == maxPage || (current + 3) >= maxPage =
        X.Element "li" [("class", "disabled")] [spanTag arrows]
    | otherwise =
        X.Element "li" [] [a url arrows]
    where arrows = [X.TextNode "»"]

li :: Int -> Int -> T.Text -> X.Node
li currentPage pageNo url =
    X.Element "li" classAttr [childTag]
    where isCurrent = currentPage == pageNo
          classAttr = if isCurrent then [("class", "active")] else []
          childTag  = if isCurrent
                          then spanTag (content:srOnly)
                          else a url [content]
          content   = X.TextNode . tshow $ succ pageNo
          srOnly    = [ X.TextNode " "
                      , spanTag' "sr-only" [X.TextNode "(current)"]
                      ]

a :: T.Text -> [X.Node] -> X.Node
a href = X.Element "a" [("href", href)]

spanTag :: [X.Node] -> X.Node
spanTag = X.Element "span" []

spanTag' :: T.Text -> [X.Node] -> X.Node
spanTag' clss = X.Element "span" [("class", clss)]

