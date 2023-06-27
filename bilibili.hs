{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Applicative (empty)
import System.ProgressBar
import GHC.Generics (Generic)
import Conduit
import Network.HTTP.Simple
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Conduit.Combinators (iterM)

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

progressStyle :: Style s
progressStyle = Style
  { styleOpen    = "["
  , styleClose   = "]"
  , styleDone    = '='
  , styleCurrent = '>'
  , styleTodo    = '.'
  , stylePrefix  = msg "Bilibili"
  , stylePostfix = percentage
  , styleWidth   = ConstantWidth 80
  , styleEscapeOpen    = const ""
  , styleEscapeClose   = const ""
  , styleEscapeDone    = const ""
  , styleEscapeCurrent = const ""
  , styleEscapeTodo    = const ""
  , styleEscapePrefix  = const ""
  , styleEscapePostfix = const ""
  , styleOnComplete = WriteNewline
  }

showProgress :: IO (ProgressBar ()) -> ByteString -> IO (IO (ProgressBar ()))
showProgress iop input = do
  p <- iop
  incProgress p (S.length input)
  return $ return p

main :: IO ()
main = do
  response <- httpJSON "https://api.bilibili.com/x/player/playurl?bvid=BV1YW4y197Pe&cid=1162298090&fnval=16"

  let resp = (getResponseBody response :: DashUrl)
  let url = (baseUrl . head . video . dash . datum) resp

  request' <- parseRequest url
  let request = setRequestMethod "GET"
              $ setRequestHeader "Referer" ["https://www.bilibili.com"]
              $ setRequestHeader "User-Agent" ["Wget/1.21.3"]
              $ request'

  httpSink request $ \r -> do
    let total = read . filter ('"' /=) . show . head $ getResponseHeader "Content-Length" r :: Int
    _ <- iterM (S.appendFile "video.m4s")
      .| foldMC showProgress (newProgressBar progressStyle 10 (Progress 0 total ()))
    sinkNull

  return ()