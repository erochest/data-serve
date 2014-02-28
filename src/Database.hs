{-# LANGUAGE OverloadedStrings #-}


module Database
    ( pgsEnvInit
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Pool
import qualified Data.Text                     as T
import           Data.Word
import           Database.PostgreSQL.Simple
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple


-- Had to re-write these to avoid using configurator
description :: T.Text
description = "PostgreSQL abstraction"

datadir :: Maybe (IO FilePath)
datadir = Nothing

pgsEnvInit :: [(T.Text, T.Text)] -> SnapletInit b Postgres
pgsEnvInit = makeSnaplet "postgresql-simple" description datadir . initHelper

readConfig :: [(T.Text, T.Text)] -> Maybe ConnectInfo
readConfig params =   ConnectInfo
                  <$> tlookup "host"
                  <*> (Just . fromMaybe (5432 :: Word16)
                            . fmap tread
                            $ lookup "port" params)
                  <*> tlookup "user"
                  <*> tlookup "password"
                  <*> tlookup "dbname"
    where tread   = read . T.unpack
          tlookup = fmap T.unpack . flip lookup params

initHelper :: (Functor m, MonadIO m) => [(T.Text, T.Text)] -> m Postgres
initHelper config = do
    conn <- maybe (fail "Invalid configuration") return $ readConfig config
    fmap Postgres . liftIO $ createPool (connect conn) close stripes
                                        (realToFrac (idle :: Double))
                                        resources
    where stripes   = 1
          idle      = 5
          resources = 20

