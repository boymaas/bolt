module Bolt.Plugin.Routing (plugin,prParams) where

import Bolt.Plugin.Base

import Control.Applicative
import Control.Arrow

import Text.RegexPR

import Network.CGI

import Data.Maybe
import Data.List

thisPluginName = "routing"

plugin ::  [PrRouteDefinition] -> PbPluginDefinition
plugin pages
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdHookRequest=Just _routeRequest
      , pbPdConfig=pcEmptyPcRoute { pcRPages = pages }
    }

_routeRequest ::  BtBolt ()
_routeRequest = do
  pages <- bshConfigAttr thisPluginName pcRPages
  -- match page
  (params, matchedPage) <- _pageMatch pages <$> bshCgiGetVar "REQUEST_URI"

  -- TODO: store params in state
  _pconfModify $ \c ->
    c { pcRParams = map snd $ snd params }

  -- execute page
  matchedPage 

prParams = _confAttr pcRParams

_pageMatch routes path = 
  extract $ look $ map (first match) routes 
  where
    match regex = matchRegexPR regex path
    look = find (isJust . fst)
    extract = maybe ( undefined, _noRouteMatch ) (first fromJust)

_noRouteMatch :: BtBolt ()
_noRouteMatch = do
  uri <- bshCgiGetVar "REQUEST_URI" 
  bshOutput ( "No route match: `" ++ uri ++ "'" )
  return ()

_pconfModify = bshConfigModify thisPluginName

_confAttr f = _conf >>= return . f
_conf = bshConfig thisPluginName
