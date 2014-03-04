{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Model
    ( DataView(..)
    , InternetUser(..)
    , internetUserId
    , internetUserCountry
    , internetUserCountryCode
    , internetUserSerCode
    , internetUserSerName
    , internetUserYear
    , internetUserCount
    ) where


import           Control.Applicative
import           Control.Lens
import qualified Data.Text                     as T
import           Snap.Snaplet.PostgresqlSimple


data DataView = DataView
              { dataViewCountry :: T.Text
              , dataViewPage    :: Int
              } deriving (Show)

data InternetUser = InternetUser
                  { _internetUserId          :: Int
                  , _internetUserCountry     :: T.Text
                  , _internetUserCountryCode :: T.Text
                  , _internetUserSerCode     :: T.Text
                  , _internetUserSerName     :: T.Text
                  , _internetUserYear        :: T.Text
                  , _internetUserCount       :: Maybe Double
                  } deriving (Show)
makeLenses ''InternetUser

instance FromRow InternetUser where
    fromRow =   InternetUser
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

