{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bolt.Types where

import qualified Bolt.Plugin.XHtmlTemplate.Types as XHtml

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Concurrent

import Network.CGI
import Network.CGI.Monad

import Database.HDBC.Sqlite3 as Sqlite3
import Database.HDBC.PostgreSQL as PostgreSQL

import qualified Data.Map as M

import Text.StringTemplate
import Text.StringTemplate.Classes (SElem)
import qualified Text.XHtml as X

newtype BtBolt a = BtBolt { runBolt :: (BtRequestStateT (CGIT IO) a) }
  deriving (Monad, MonadState BsRequestState, MonadIO, Functor)

type BtRequestStateT m a = StateT BsRequestState m a

instance Applicative (BtBolt) where
    pure = return
    (<*>) = ap

instance MonadCGI (BtBolt) where
  cgiAddHeader n v = BtBolt $ lift $ cgiAddHeader n v
  cgiGet x = BtBolt $ lift $ cgiGet x


-- state
data BsRequestState = BsRequestState {
    -- idea is that output of all
    -- plugins is redirected to the 
    -- bsRsOutput variable which will
    -- be output in the end of the chain
    -- this way postfilters will be able 
    -- to work
    bsRsOutput :: Maybe String

    -- pluginconfigs are stored in the BsRequestState
  , bsPluginConfigMap :: M.Map String PcConfigType
} 

bsEmptyBsRequestState = BsRequestState { 
      bsRsOutput=Nothing
    , bsPluginConfigMap=M.empty
}

-- configuration for Plugins
data PcConfigType = PcHStringTemplate {
                      pcHstTmplDir :: String
                   ,  pcHstTmplGroup :: Maybe (STGroup String)
                   ,  pcHstTmpl :: Maybe (StringTemplate String)
                   ,  pcHstTmplVars :: M.Map String (SElem String)
                        }
                   | PcRoutes { pcRPages :: [(String,BtBolt ())]
                              , pcRParams :: [String] }
                   | PcSession { 
                       pcSSessionStorage :: Maybe (MVar PcSessionStorage)
                   ,   pcSSessionData :: Maybe PcSessionData
                   ,   pcSSessionDataStart :: Maybe PcSessionData
                   ,   pcSSessionId :: Maybe Integer 
                     }
                   | PcSessionPostgreSQL { 
                       pcSPSessionConnString :: String
                   ,   pcSPSessionConn :: Maybe PostgreSQL.Connection 
                   ,   pcSPSessionData :: Maybe PcSessionData
                   ,   pcSPSessionDataStart :: Maybe PcSessionData
                   ,   pcSPSessionId :: Maybe Integer 
                     }
                   | PcHDBCPostgreSQL { 
                       pcHPSqlConnection :: Maybe PostgreSQL.Connection 
                   ,   pcHPSqlConnectionString :: Maybe String
                     }
                   | PcHDBCSqlite3 { 
                       pcHSqlite3Connection :: Maybe Sqlite3.Connection 
                   ,   pcHDbFile :: Maybe String
                     }
                   | PcSerializable String
                   | PcMap (M.Map String String)
                   | PcXHtmlTemplate {
                       pcXtVars :: XHtml.TmplVars
                     }
                   | PcEmpty

type PcSessionStorage = (M.Map Integer PcSessionData)
type PcSessionData    = (M.Map String String)

pcEmptyPcHStringTemplate = PcHStringTemplate undefined Nothing Nothing M.empty
pcEmptyPcRoute = PcRoutes [] []
pcEmptyPcSession = PcSession Nothing Nothing Nothing Nothing
pcEmptyPcHDBCSqlite3 = PcHDBCSqlite3 Nothing Nothing
pcEmptyPcHDBCPostgreSQL = PcHDBCPostgreSQL Nothing Nothing 
pcEmptyPcSessionPostgreSQL = PcSessionPostgreSQL "" Nothing Nothing Nothing Nothing
