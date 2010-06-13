module Bolt.Plugin.XHtmlTemplate.Types (
  TmplVars
, XHtmlLayout
, module Bolt.Plugin.XHtmlTemplate.TmplVars 
) where

import Bolt.Plugin.XHtmlTemplate.TmplVars 

import qualified Data.Map as M
import qualified Text.XHtml as X

type TmplVars = M.Map String TmplVar

type XHtmlLayout = (TmplVars -> X.Html)

