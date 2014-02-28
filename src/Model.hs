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


import           Control.Lens
import qualified Data.Text    as T


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
                  , _internetUserCount       :: Double
                  } deriving (Show)
makeLenses ''InternetUser

