{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString)
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Web.Heroku
------------------------------------------------------------------------------
import           Application
import           Database
import           Form


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",       ifTop (redirect "/data") <|> serveDirectory "static")
         , ("/about", render "about/index")
         , ("/data",  with pg runDataViewForm)
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

