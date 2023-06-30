{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Data.Text.Lazy (pack)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

--
-- Json data definitions to resolve resource url
--

data Video = Video
  { videoId  :: Int
  , videoUrl :: String
  }
  deriving (Eq, Show)

instance FromJSON Video where
  parseJSON (Object v) = Video <$> v .: "id" <*> v .: "base_url"
  parseJSON _ = empty

data Audio = Audio
  { audioId  :: Int
  , audioUrl :: String
  }
  deriving (Eq, Show)

instance FromJSON Audio where
  parseJSON (Object v) = Audio <$> v .: "id" <*> v .: "base_url"
  parseJSON _ = empty

data Dash = Dash
  { video :: [Video]
  , audio :: [Audio]
  }
  deriving (Eq, Show, Generic, FromJSON)

newtype Datum = Datum { dash :: Dash } deriving (Eq, Show, Generic, FromJSON)

newtype DashUrl = Stuff
  { datum :: Datum } deriving (Eq, Show)

instance FromJSON DashUrl where
  parseJSON (Object v) = Stuff <$> v .: "data"
  parseJSON _ = empty

fetchVideoUrl :: DashUrl -> String
fetchVideoUrl = videoUrl . head . video . dash . datum

fetchAudioUrl :: DashUrl -> String
fetchAudioUrl = audioUrl . head . audio . dash . datum

fetchResource :: String -> FilePath -> IO ()
fetchResource url ofile = do
  putStrLn url

  request' <- parseRequest url
  let request = setRequestMethod "GET"
              . setRequestHeader "Referer" ["https://www.bilibili.com"]
              . setRequestHeader "User-Agent" ["Wget/1.21.3"]
              $ request'

  let style = setProgressStylePrefix (msg $ pack ofile) progressStyle

  httpSink request $ \r -> do
    let total = read . filter ('"' /=) . show . head $ getResponseHeader "Content-Length" r :: Int
    _ <- iterM (S.appendFile ofile)
      .| foldMC showProgress (newProgressBar style 10 (Progress 0 total ()))
    sinkNull

--
-- progress bar utils
--

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

setProgressStylePrefix :: Label s -> Style s -> Style s
setProgressStylePrefix prefix s = s { stylePrefix = prefix } 

showProgress :: IO (ProgressBar ()) -> ByteString -> IO (IO (ProgressBar ()))
showProgress iop input = do
  p <- iop
  incProgress p (S.length input)
  return $ return p

main :: IO ()
main = do
  let api  = "https://api.bilibili.com/x/player/playurl"

  let bvid  = "BV1YW4y197Pe"
  let cid   = "1162298090"
  let query = [("bvid", Just bvid), ("cid", Just cid), ("fnval", Just "16")]

  req' <- parseRequest api
  let req = setRequestMethod "GET"
          . setRequestQueryString query
          $ req'

  response <- httpJSON req

  let playurl = (getResponseBody response :: DashUrl)

  fetchResource (fetchVideoUrl playurl) ((unpack . decodeUtf8) bvid ++ ".video.m4s")
  fetchResource (fetchAudioUrl playurl) ((unpack . decodeUtf8) bvid ++ ".audio.m4s")

  return ()