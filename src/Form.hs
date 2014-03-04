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
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Debug.Trace
import           Heist
import qualified Heist.Interpreted             as I
import           Model
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Text.Digestive
import           Text.Digestive.Heist


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
runGetDataView prefix form = postForm prefix form . env =<< getParams

env :: Monad m => Params -> FormEncType -> m (Env m)
env ps _ = return ( maybe (fail "") return
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
                   $  mapMaybe listToMaybe countries
    maybe (renderForm view) renderDataView result

renderDataView :: DataView -> Handler App Postgres ()
renderDataView DataView{..} = do
    iusers :: [InternetUser] <- query "SELECT *\
                    \ FROM internet_users\
                    \ WHERE country=?\
                    \ ORDER BY id\
                    \ LIMIT ?\
                    \ OFFSET ?;"
                    (dataViewCountry, pageSize, dataViewPage * pageSize)
    renderWithSplices "iuser_table" $ do
        "iusers" ## renderInternetUsers iusers

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

