module Bolt.Plugin.Session (plugin,pSSessionStore,pSSessionGet,pSSessionExists) where

import Bolt.Plugin.Base

import Control.Applicative
import Control.Arrow
import Control.Concurrent

import Network.CGI

import Data.Maybe
import Data.List

import System.IO
import System.Random
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Data.Map as M

-- Session handling plugin
--
--  SessionStorage MVar contains all session and such is:
--
--  Map Integer (Map string string)
--    i> only Stings of types of Show and Read can be stored
-- 
-- Init
--    create new MVar with empty SessionStorage

-- Prerequest 
--    SessionStorage <- readMVar (non blocking)
--    load SessionId from cookie if present (Maybe)
--    if no SessionId:
--      load empty map
--    else
--      lookup sessionData from session storage
--        + -> store sessionData in PcSession
--        - -> store empty in PcSession
--  
-- Postrequest 
--    if session map has been changed.
--      loadSessionId from cookie
--      if no cookieSessionId is present:
--        generate sessionId
--
--        set headers 
--
--      store sessionData in SessionStorage
--      store session inside MVar using (modifyMVar_) non-blocking
--

thisPluginName = "session"
gCookieId = "boltsid"

plugin :: PbPluginDefinition
plugin
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdHookInitialize=_initialize
      , pbPdHookPreRequest=Just _preRequest
      , pbPdHookPostRequest=Just _postRequest
    }

_initialize :: BsPluginInitializer
_initialize p = do
  sessionStorage <- newMVar M.empty
  return $ pcEmptyPcSession { pcSSessionStorage = Just sessionStorage }

_preRequest :: BtBolt ()
_preRequest = do
  sessionId <- readCookie gCookieId
  sessionData <- case sessionId of
    Just id -> get_from_storage id
    Nothing -> return M.empty

  -- store sessionData inside pluginconfig
  _pconfModify (\pconf -> pconf { pcSSessionData = Just sessionData, pcSSessionDataStart = Just sessionData } )
  return ()

  where 
    get_from_storage id = do
      sessionStorage <- _readMVarSessionStorage
      return $ fromMaybe M.empty (M.lookup id sessionStorage)

    _readMVarSessionStorage = _pconfAttr pcSSessionStorage >>= liftIO . readMVar . fromJust >>= return


_postRequest ::  BtBolt ()
_postRequest = _whenSessionChanged $ do
  cookieSessionId <- readCookie gCookieId
  sessionId <-
    case cookieSessionId of 
      Just id -> return id
      Nothing -> do 
        sessionId' <- head <$> liftIO _generateSessionId
        setCookie ( newCookie gCookieId $ show sessionId'  ) { cookiePath = Just "/" }
        return sessionId'

  -- store sessiondata in mvar
  sessionStorage <- fromJust <$> _pconfAttr pcSSessionStorage
  sessionData    <- fromJust <$> _pconfAttr pcSSessionData

  liftIO $ modifyMVar_ sessionStorage $ \ sessionStorage' ->
    return $ M.insert sessionId sessionData sessionStorage'

  return ()
  where
    _whenSessionChanged block = do
      sessionStart <- _pconfAttr pcSSessionDataStart
      session <- _pconfAttr pcSSessionData
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
_sessionData = fromJust <$> _pconfAttr pcSSessionData

pSSessionStore k v = do
  session <- _sessionData
  _pconfModify (\pconf -> pconf { pcSSessionData = Just (M.insert k v session) } )
  

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
