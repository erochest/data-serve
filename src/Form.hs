{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Form
    ( dataViewForm
    , runDataViewForm
    ) where


import           Application
import           Control.Applicative
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Heist.Interpreted             as I
import           Model
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap


dataViewForm :: Monad m => [T.Text] -> Form T.Text m DataView
dataViewForm countries =
        DataView
    <$> "country" .: choice countries' Nothing
    <*> "page"    .: stringRead "" (Just 0)
    where countries' = map (\c -> (c, c)) countries

runDataViewForm :: Handler App Postgres ()
runDataViewForm = do
    countries <- query_ "SELECT DISTINCT country\
                        \ FROM internet_users\
                        \ ORDER BY country";
    (view, result) <- runForm "data"
                   .  dataViewForm
                   $  mapMaybe listToMaybe countries
    maybe (renderForm view) renderDataView result

renderDataView :: DataView -> Handler App Postgres ()
renderDataView = undefined

renderForm :: View T.Text -> Handler App Postgres ()
renderForm view =
    heistLocal (bindDigestiveSplices view) $ render "country_name_form"

