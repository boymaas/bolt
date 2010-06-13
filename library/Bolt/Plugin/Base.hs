-- | defines everything needed to create a plugin
module Bolt.Plugin.Base (
   BsPluginInitializer
,  PluginHook
,  PrRouteDefinition
, PbPluginDefinition (..)
,  emptyPlugin
,  module Bolt.Types
,  module Bolt.Shared
) where 

-- plugins will run in 2 context
-- the initialize will run in a state monad
-- for building up the initial state
-- which will be passed to the Bolt server
--
-- the rest will run inside the CGI Monad
-- only reading the state. Changes towards
-- the state will only exists for one request
-- unless some kind of storage will be used.

import Network.CGI (CGI,CGIResult)
import Network.CGI.Monad (CGIT)

import Bolt.Types
import Bolt.Shared

import qualified Data.Map as M

-- functions that change Request state
type BsPluginInitializer = (PcConfigType -> IO PcConfigType)
type PluginHook a = (PbPluginDefinition -> Maybe a)

-- route definition
type PrRouteDefinition = (String, BtBolt ())

-- | Every plugin has to define this structure. This structure will
-- be used to initialize, run and destroy the plugin.
data PbPluginDefinition = PbPluginDefinition {
    pbPdName :: String                        -- ^ Name of plugin, should only contain [A-Za-z_] characters.
  , pbPdHookInitialize :: BsPluginInitializer -- ^ function to initialize the configuration. As to initialize a database connection. Takes PcConfigType
  , pbPdHookPreRequest :: Maybe (BtBolt ())   -- ^ function with no arguments to be run PreRequest
  , pbPdHookRequest :: Maybe (BtBolt ())      -- ^ function to handle Request
  , pbPdHookPostRequest :: Maybe (BtBolt ())  -- ^ function to handle PostRequest
  , pbPdHookDestroy :: BsPluginInitializer    -- ^ function to free allocated resources by the plugin
  , pbPdConfig :: PcConfigType                -- ^ The configuration associated with this plugin
  , pbPdRoutes :: [PrRouteDefinition]         -- ^ Routes to be registered by this plugin. Plugins can be fullblown applications.
}

-- | Standard empty plugindefinition
emptyPlugin = PbPluginDefinition { 
    pbPdName="empty" 
  -- default initializer just passes on the configtype
  , pbPdHookInitialize = (\pc -> return pc)
  , pbPdHookPreRequest = Nothing
  , pbPdHookRequest = Nothing
  , pbPdHookPostRequest = Nothing
  , pbPdHookDestroy = (\pc -> return pc)
  , pbPdConfig = PcEmpty
  , pbPdRoutes = []
}

