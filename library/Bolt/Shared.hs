module Bolt.Shared where

import Control.Monad.State
import Control.Applicative
import Control.Exception (assert)
import Bolt.Types

import Network.CGI

import Data.Maybe

import qualified Data.Map as M

-- utility function to store output in state
bshOutput ::  String -> BtBolt ()
bshOutput o = do
  modify (\s -> s { bsRsOutput = Just o })

bshHasOutput ::  BtBolt Bool
bshHasOutput = 
  gets bsRsOutput >>= return . isJust

-- finds config, assumes its there
bshConfig :: String -> BtBolt PcConfigType
bshConfig name = do
  configmap <- gets bsPluginConfigMap
  -- need to check for this .. since its needed

  case (M.member name configmap) of
    False -> error $ "Trying to access configuration of plugin that \
                     \has not been registered. Namely: " ++ name
    True -> return . fromJust $ M.lookup name configmap

bshConfigAttr name f = do
  c <- bshConfig name
  return $ f c

-- install new configuration
bshConfigUpdate name conf = do
  configmap <- gets bsPluginConfigMap
  modify (\s -> s { bsPluginConfigMap = M.insert name conf configmap })

bshConfigModify :: String -> (PcConfigType -> PcConfigType) -> BtBolt ()
bshConfigModify name f = do
  c <- bshConfig name
  bshConfigUpdate name (f c)

bshCgiGetVar :: String -> BtBolt String
bshCgiGetVar s = getVar s >>= return . fromMaybe ""
