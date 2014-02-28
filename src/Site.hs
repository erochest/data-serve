{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
-- import           Control.Applicative
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString)
-- import           Data.Pool
-- import qualified Data.Text                                   as T
-- import           Database.PostgreSQL.Simple
-- import           Heist
-- import qualified Heist.Interpreted                           as I
-- import           Snap.Core
import           Snap
import           Snap.Snaplet.Heist
-- import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
-- import           Text.Digestive
-- import           Text.Digestive.Heist
-- import           Text.Digestive.Snap
import           Web.Heroku
------------------------------------------------------------------------------
import           Application
import           Database
import           Form
-- import           Model


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("", ifTop (redirect "/form") <|> serveDirectory "static")
         , ("/form", with pg runDataViewForm)
         , ("/about", render "about/index")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    p <- nestSnaplet "pg" pg . pgsEnvInit =<< liftIO dbConnParams

    addRoutes routes
    return $ App h s p

