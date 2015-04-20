{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State           (get)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _pg    :: Snaplet Postgres
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get


------------------------------------------------------------------------------
type AppHandler = Handler App App


