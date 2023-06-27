{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Applicative (empty)
import System.IO ( stdout )
import System.ProgressBar
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.Conduit.List      as CL
import Conduit
import Network.HTTP.Simple
import GHC.Generics (Generic)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Conduit as CL
-- import qualified Data.ByteString.Lazy.Char8 as L8

data Video = Video
  { videoId :: Int
  , baseUrl :: String
  }
  deriving (Eq, Show)

instance FromJSON Video where
  parseJSON (Object v) = Video <$> v .: "id" <*> v .: "base_url"
  parseJSON _ = empty

newtype Dash = Dash { video :: [Video] } deriving (Eq, Show, Generic, FromJSON)

newtype Datum = Datum { dash :: Dash } deriving (Eq, Show, Generic, FromJSON)

newtype DashUrl = Stuff
    { datum :: Datum } deriving (Eq, Show)

instance FromJSON DashUrl where
  parseJSON (Object v) = Stuff <$> v .: "data"
  parseJSON _ = empty

processed :: Int -> ByteString -> IO Int
processed total input = do
  let n = S.length input
  return $ n + total

main :: IO ()
main = do
  response <- httpJSON "https://api.bilibili.com/x/player/playurl?bvid=BV1YW4y197Pe&cid=1162298090&fnval=16"
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
  print (getResponseHeader "Content-Type" response)

  let resp = (getResponseBody response :: DashUrl)

  let url = (baseUrl . head . video .dash . datum) resp

  print url

  request'  <- parseRequest url
  let request = setRequestMethod "GET"
              $ setRequestHeader "Referer" ["https://www.bilibili.com"]
              $ setRequestHeader "User-Agent" ["Wget/1.21.3"]
              $ request'

  -- response' <- httpLBS request
  -- putStrLn $ "The status code was: " ++ show (getResponseStatusCode response')
  -- print (getResponseHeader "Content-Type" response')
  -- print (getResponseHeader "Content-Length" response')

  httpSink request $ \r-> do
      liftIO $ putStrLn
             $ "The status code was: " ++ show (getResponseStatusCode r)

      liftIO $ print (getResponseHeader "Content-Length" r)

      -- let total = read $ show $ head $ getResponseHeader "Content-Length" r :: Int

      -- let pb = newProgressBar defStyle 10 (Progress 0 total ())

      -- lengthC
      mapMC (S.hPut stdout)

  return ()