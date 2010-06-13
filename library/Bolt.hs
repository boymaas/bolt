module Bolt (
  -- * Main functions
   bStartBolt 
  ,bRunBoltScript) where

import Bolt.Plugin.Base
import Bolt.Plugin.Routing as Routing

import Network.CGI
import Network.CGI.Protocol
import Network.CGI.Monad
import Network.FastCGI

import Control.Concurrent
import Control.Monad.State
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Map as M


bStartBolt ::  [PbPluginDefinition] -> [PrRouteDefinition] -> IO ()
-- ^ starting the bolt server using a plugin definition, which are defined
-- in the seperate plugin modules. This functions adds the Routing plugin
-- module per standard. Since user made plugins may define routing information
-- as part of their definition
bStartBolt plugins' routes = do 
  -- routing enabled by default
  -- with all defined routes from all plugins
  -- of all plugins (makes an app an plugin)
  let plugins = plugins' ++ [Routing.plugin $ routes ++ (concatMap pbPdRoutes $ plugins)]

  -- build initial state
  initState <- _initBolt plugins
  -- run pre,post,in request handlers
  runFastCGIConcurrent' forkIO 1000 $ _runMain initState plugins
  -- destroy buildup state
  _destroyBolt initState plugins
  return ()

bRunBoltScript ::  [PbPluginDefinition] -> BtBolt () -> IO ()
-- ^ running a bolt script without the webserver. One can define
-- several plugins to be configured before the script is executed. May be handy
-- to use the database functionality.
bRunBoltScript plugins script = do
  -- build initial state
  initState <- _initBolt plugins
  -- run pre,post,in request handlers
  runCGI $ _runMain initState (plugins ++ [emptyPlugin {pbPdHookRequest = Just script}])
  -- destroy buildup state
  _destroyBolt initState plugins
  return ()

_initBolt ::  [PbPluginDefinition] -> IO BsRequestState
_initBolt plugins = 
  _runPluginInitializers bsEmptyBsRequestState pbPdHookInitialize plugins 

_runMain ::  BsRequestState -> [PbPluginDefinition] -> CGIT IO CGIResult
_runMain initState plugins = do 
  -- run pre,in,post handlers of plugins
  preReqState  <- _runPluginHooks initState   $ _filterDefinedHooks pbPdHookPreRequest plugins
  inReqState   <- _runPluginHooks preReqState $ _filterDefinedHooks pbPdHookRequest plugins
  postReqState <- _runPluginHooks inReqState  $ _filterDefinedHooks pbPdHookPostRequest plugins
  -- return output as stored inside state
  output $ fromMaybe "" $ bsRsOutput postReqState  

_destroyBolt ::  BsRequestState -> [PbPluginDefinition] -> IO BsRequestState
_destroyBolt state plugins = 
  _runPluginInitializers state pbPdHookDestroy plugins

_runPluginHooks :: BsRequestState -> [BtBolt a] -> CGIT IO BsRequestState
_runPluginHooks state hooks = 
  acc state hooks
  where acc s (h:hs) = do 
          (_,s') <- _runHook h s
          acc s' hs
        acc s [] = return s

_runHook ::  BtBolt a -> BsRequestState -> CGIT IO (a, BsRequestState)
_runHook h st = 
  runStateT (runBolt h) st

_runPluginInitializers ::  BsRequestState -> (PbPluginDefinition -> BsPluginInitializer) -> [PbPluginDefinition] -> IO BsRequestState
_runPluginInitializers initState pdHook plugins = 
  acc initState (filter' plugins) 
  where acc state ((hook,plugin):hps) = do 
             pconfig <- hook $ pbPdConfig plugin
             -- add initialized plugin to state
             let state' = state { bsPluginConfigMap = (M.insert (pbPdName plugin) pconfig (bsPluginConfigMap state)) }
             acc state' hps
        acc state [] = return state

        filter' = map (\p -> ((pdHook p), p)) 

_filterDefinedHooks ::  PluginHook a -> [PbPluginDefinition] -> [a]
_filterDefinedHooks pdHook plugins = 
  map fromJust . filter isJust . map pdHook $ plugins

