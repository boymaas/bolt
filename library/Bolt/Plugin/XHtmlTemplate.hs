module Bolt.Plugin.XHtmlTemplate where

-- Plugin to ease working with XHtml generated content
-- this plugin assumes all layouts can be rendered providing
-- an XHtml function with a state. Hence the XHtmlLayout type.
--
-- In this case the state is generalised as a Map String Html
-- making implementation relatively simple.
-- 
-- All vars will be stored inside plugin configuration
-- on render they will be gathered making it possible
-- to define variables in differen BtBolt functions

import Bolt.Plugin.Base

import Control.Applicative

import qualified Text.XHtml as X
import qualified Data.Map as M
import Data.Maybe

import Bolt.Plugin.XHtmlTemplate.Types

thisPluginName = "xhtmltemplate"

plugin
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdConfig=PcXHtmlTemplate { pcXtVars = M.empty }
    }

-- helper
tv a b = (a, toTmplVar b)

pxtTmplVar :: (ToTmplVar tv) => String -> tv -> BtBolt ()
pxtTmplVar k v = 
  _confModify $ \c ->
    -- insert into global config
    c { pcXtVars = M.insert k (toTmplVar v) (pcXtVars c) }

pxtTmplVarMany :: (ToTmplVar tv) => [(String, tv)] -> BtBolt ()
pxtTmplVarMany list = _setMany pxtTmplVar list

_setMany f list = mapM_ (\(k,v) -> f k v ) list 

-- return configured map
pxtTmplMap ::  BtBolt TmplVars
pxtTmplMap = bshConfigAttr thisPluginName pcXtVars

-- render to output
pxtRenderOutput :: XHtmlLayout -> BtBolt ()
pxtRenderOutput layout = do
  pxtRender layout >>= bshOutput

-- render to string
pxtRender :: XHtmlLayout -> BtBolt String
pxtRender layout = do
  tmplmap <- pxtTmplMap
  return $ _renderHtml (layout tmplmap)

-- render fragment
pxtRenderFragment :: XHtmlLayout -> BtBolt String
pxtRenderFragment layout = do
  tmplmap <- pxtTmplMap
  return $ _renderHtmlFragment (layout tmplmap)

-- helpers
pxtContent :: (ToTmplVar tv) => tv -> BtBolt ()
pxtContent = pxtTmplVar "content"

-- shortcuts
_renderHtml = X.renderHtml
_renderHtmlFragment = X.renderHtmlFragment


_conf = bshConfig thisPluginName

_confModify :: (PcConfigType -> PcConfigType) -> BtBolt ()
_confModify = bshConfigModify thisPluginName
