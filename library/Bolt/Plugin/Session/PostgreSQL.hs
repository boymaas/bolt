module Bolt.Plugin.Session.PostgreSQL (plugin,pSSessionStore,pSSessionGet,pSSessionExists,pSSessionClear) where

import Bolt.Plugin.Base

import Control.Applicative
import Control.Arrow
import Control.Concurrent

import Network.CGI

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Time

import System.IO
import System.Random
import Foreign.Marshal.Alloc
import Foreign.Storable

import Database.HDBC
import Database.HDBC.PostgreSQL

import Bolt.Plugin.HDBC.SqlAbstraction

thisPluginName = "Session.PostgreSQL"
gCookieId = "BoltSID"

plugin :: String -> PbPluginDefinition
plugin connString 
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdHookInitialize=_initialize
      , pbPdHookPreRequest=Just _preRequest
      , pbPdHookPostRequest=Just _postRequest
      , pbPdConfig = pcEmptyPcSessionPostgreSQL { pcSPSessionConnString = connString }
    }

_initialize :: BsPluginInitializer
_initialize p = do
  conn <- connectPostgreSQL $ pcSPSessionConnString p

  -- create table when not exists
  tables <- getTables conn
  when ("bolt_session" `notElem` tables) $ do 
    createSessionTable conn

  return $ p { pcSPSessionConn = Just conn }

when a b = 
  case a of
    True -> b >> return ()
    False -> return ()

createSessionTable conn = do
  liftIO $ run conn "create table bolt_session ( session_id varchar(255) NOT NULL, data bytea, last_access timestamp with time zone, primary key ( session_id ) )" []
  liftIO $ commit conn

_preRequest :: BtBolt ()
_preRequest = do
  sessionId <- getCookie gCookieId
  sessionData <- case sessionId of
    Just id -> get_from_storage id
    Nothing -> return M.empty

  -- TODO: clean expired sessions every hour
    -- expired session is last access time

  -- store sessionData inside pluginconfig
  _pconfModify (\pconf -> pconf { pcSPSessionData = Just sessionData, pcSPSessionDataStart = Just sessionData } )
  return ()

  where 
    get_from_storage :: String -> BtBolt (M.Map String String)
    get_from_storage id = do
      conn <- fromJust <$> _pconfAttr pcSPSessionConn
      r <- liftIO $ query' conn $ select_where "bolt_session" ["data", "last_access"] "session_id = ?" [toSql id]
      -- read data from database
      case null r of
        -- if exists
        False -> do
          let r0 = head r
          let sessionData = fsql $ r0 !! 0
          let lastAccess  = fsql $ r0 !! 1

          cTime <- liftIO $ getCurrentTime

          -- if access time < 1h
          when (diffUTCTime cTime lastAccess > 60*60 ) $ do
            -- update ping so cleaner knows we are still using it
            liftIO $ run conn "update bolt_session set last_access = ? where session_id = ?" [tsql cTime, tsql id]
            liftIO $ commit conn
           
            
          -- deserialize into Map 
          return $ M.fromList (read sessionData :: [(String,String)])

        -- doesn't exist return empty map
        True -> return M.empty

_postRequest ::  BtBolt ()
_postRequest = _whenSessionChanged $ do
  conn <- fromJust <$> _pconfAttr pcSPSessionConn
  cookieSessionId <- readCookie gCookieId
  sessionId <-
    case cookieSessionId of 
      Just id -> return id
      Nothing -> do 
        sessionId' <- head <$> liftIO _generateSessionId
        setCookie ( newCookie gCookieId $ show sessionId'  ) { cookiePath = Just "/" }
        return sessionId'

  -- store sessiondata
  sessionData    <- fromJust <$> _pconfAttr pcSPSessionData

  -- serialize and store into 
  -- upsert database
  cTime <- liftIO $ getCurrentTime
  liftIO $ upsert conn runInsert "bolt_session" [ucid "session_id" sessionId, uc "data" (show $ M.toList sessionData), uc "last_access" cTime]
  liftIO $ commit conn

  return ()
  where
    _whenSessionChanged block = do
      sessionStart <- _pconfAttr pcSPSessionDataStart
      session <- _pconfAttr pcSPSessionData
      case sessionStart /= session of
        True -> block
        False -> return ()


-- random session ids
_generateSessionId :: IO [Integer]
_generateSessionId = randomRs (1,1000^(20::Int)) <$> _betterStdGen

-- A better random number generator which uses /dev/random when entropy
-- is available
_betterStdGen :: IO StdGen
_betterStdGen = alloca $ \p -> do
    h <- openBinaryFile "/dev/urandom" ReadMode
    hGetBuf h p $ sizeOf (undefined :: Int)
    hClose h
    mkStdGen <$> peek p


-- exported member functions
_sessionData = fromJust <$> _pconfAttr pcSPSessionData

pSSessionClear k = do
  session <- _sessionData
  _pconfModify (\pconf -> pconf { pcSPSessionData = Just (M.delete k session) } )

pSSessionStore k v = do
  session <- _sessionData
  _pconfModify (\pconf -> pconf { pcSPSessionData = Just (M.insert k v session) } )
  

pSSessionGet k = do
  session <- _sessionData
  return $ M.lookup k session


pSSessionExists k = do
  session <- _sessionData
  return $ M.member k session

-- config accessors
_pconfAttr = bshConfigAttr thisPluginName
_pconf = bshConfig thisPluginName
--_pconfUpdate = bshConfigUpdate thisPluginName
_pconfModify = bshConfigModify thisPluginName

-- sql helper
-- since we have sessionid we don't need to return any id
runInsert :: (IConnection conn) => conn -> InsertStatement -> IO Integer
runInsert conn (stmnt, values, table) = do
  runSql conn (stmnt, values)
  return 0
