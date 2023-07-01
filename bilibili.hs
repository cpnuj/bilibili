{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import GHC.Generics (Generic)
import Control.Applicative (empty)
import Control.Concurrent.Async (concurrently_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Conduit (runResourceT, sinkFile)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
-- import Data.Conduit.Combinators (iterM)
-- import Data.Text.Lazy (pack)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
-- import Conduit
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseHeader, setRequestHeader, setRequestMethod, httpJSON, getResponseBody, setRequestQueryString)
import System.Console.AsciiProgress
import System.Process

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

updateProgress :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgress pg = await >>= maybe (return ()) (\chunk -> do
    let len = S.length chunk
    liftIO $ tickN pg len
    yield chunk
    updateProgress pg)

fetchResource :: String -> FilePath -> IO ()
fetchResource url ofile = do
  -- putStrLn url
  manager <- newManager tlsManagerSettings
  request' <- parseRequest url
  let request = setRequestHeader "Referer" ["https://www.bilibili.com"]
              . setRequestHeader "User-Agent" ["Wget/1.21.3"]
              $ request'
  runResourceT $ do
    response <- http request manager
    let total = read . filter ('"' /=) . show . head $ getResponseHeader "Content-Length" response
    pg <- liftIO $ newProgressBar def
      { pgFormat = ofile ++ " [:bar] :percent"
      , pgTotal = total, pgWidth = 100
      , pgOnCompletion = Just $ ofile ++ " done"}
    runConduit $ responseBody response .| updateProgress pg .| sinkFile ofile
    liftIO $ complete pg

main :: IO ()
main = displayConsoleRegions $ do
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

  let bs2str = unpack . decodeUtf8
  let fvideo = bs2str bvid ++ ".video.m4s"
  let faudio = bs2str bvid ++ ".audio.m4s"
  let fall = bs2str bvid ++ ".mp4"

  let t1 = fetchResource (fetchVideoUrl playurl) fvideo
  let t2 = fetchResource (fetchAudioUrl playurl) faudio

  concurrently_ t1 t2

  _ <- createProcess $ (shell $ "ffmpeg -n -i " ++ fvideo ++ " -i " ++ faudio ++ " -codec copy " ++ fall) { std_out = NoStream, std_err = NoStream }

  return ()