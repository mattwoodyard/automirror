{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wai -- (responseLBS, Application, rawPathInfo, Response, Request, ResponseReceived)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.List as List

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Control.Concurrent

import Network.Curl.Download
import Network.Curl.Opts
import System.Directory
import qualified Data.Text as T 
import System.FilePath


data UrlPath
  = UrlDirectory [String]
  | UrlFile [String]


data Config = Config
  { cfgRootPath :: String
  , cfgAvailableMirrors :: [Mirror]
  , cfgCurlOpts :: [CurlOption]
  }

data Mirror = Mirror
  { mirrorName :: String
  , mirrorSource :: String
  }

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

newtype LocalPath = LocalPath String
  deriving (Eq, Show)

indexFile p = p </> "__index__"

mirrorForRequest :: Config -> [T.Text] -> Maybe Mirror
mirrorForRequest cfg url =
  case List.filter (\i -> List.isPrefixOf [(T.pack (mirrorName i))] url) (cfgAvailableMirrors cfg) of
    [] -> Nothing
    x:_ -> Just x

urlIsDirectory :: String -> Bool
urlIsDirectory p =
  List.isSuffixOf "/" p

downloadFile :: Config -> LocalPath -> String -> IO (MVar (Either String ()))
downloadFile cfg (LocalPath path) url = do
  doneMvar <- newEmptyMVar
  -- TODO handle error conditions
  _ <- forkIO $ do
    response <- openURIWithOpts (cfgCurlOpts cfg) url
    case response of
      Left err -> do
        putStrLn $ show err
        putMVar doneMvar (Left err)
      Right content -> do
        if urlIsDirectory url then do
          _ <- mkHeirarchy "/" (splitDirectories $ indexFile path)
          B8.writeFile (indexFile path) content
        else do
          _ <- mkHeirarchy "/" (splitDirectories path)
          B8.writeFile path content
        putMVar doneMvar $ Right ()
  return doneMvar



mkHeirarchy :: FilePath -> [String] -> IO Bool
mkHeirarchy parents remaining = do
  case remaining of
    [] -> return $ True
    _:[] ->
      return $ True
    c:cs -> do
      isdir <- (doesDirectoryExist (parents </> c))
      isfile <- (doesFileExist (parents </> c))
      case (isdir, isfile) of
        (False, False) -> do
          putStrLn $ "Make a new directory" ++ (parents </> c)
          createDirectory (parents </> c)
          mkHeirarchy (parents </> c) cs
        (True, False) -> do
          putStrLn $ "directory exists" ++ (parents </> c)
          mkHeirarchy (parents </> c) cs
        (False, True) -> do
          putStrLn $ "file exists at directory, swap around" ++ (parents </> c)
          renameFile (parents </> c) (parents </> (c ++".tmp"))
          createDirectory (parents </> c)
          renameFile (parents </> (c ++ ".tmp")) (indexFile $ parents </> c)
          mkHeirarchy (parents </> c) cs
        (True, True) -> do
          putStrLn "Complete nonsense"
          return $ False


-- TODO index handling
localPathForUrl :: Config -> Mirror -> [T.Text] -> LocalPath
localPathForUrl c m u =
  LocalPath $ (cfgRootPath c) </> (mirrorName m) </> joinPath (List.map T.unpack (List.tail u))

remotePath :: Mirror -> [T.Text] -> String
remotePath m u =
  (mirrorSource m) ++ sep ++ path_components
  where
    path_components = List.intercalate "/" $ List.map T.unpack $ List.tail u
    sep = if List.isSuffixOf "/" (mirrorSource m) then "" else "/"

readableLocalFile :: String -> IO String
readableLocalFile file_name = do
  isdir <- doesDirectoryExist file_name
  case isdir of
    True -> return $ indexFile file_name
    False ->  return $ file_name

sendFile :: (Response -> IO ResponseReceived) -> LocalPath -> IO ResponseReceived
sendFile respond (LocalPath lp) = do 
  rlp <- readableLocalFile lp
  respond $ responseFile status200 [] rlp Nothing

localPathExists :: LocalPath -> IO Bool
localPathExists (LocalPath lp) = doesFileExist lp >>= \x ->
  doesDirectoryExist lp >>= \y -> (return $ x || y)

handle :: Config -> Application
handle cfg req respond =
  case (mirrorForRequest cfg (pathInfo req)) of
    Just chosen_mirror -> do
        exist <- localPathExists lpath
        case exist of
          True -> sendFile respond lpath
          False -> do
            wait_for_download <- downloadFile cfg lpath (remotePath chosen_mirror (pathInfo req))
            q <- readMVar wait_for_download
            sendFile respond lpath
      where
        lpath = (localPathForUrl cfg chosen_mirror (pathInfo req))

    Nothing ->
        respond $ notFound


