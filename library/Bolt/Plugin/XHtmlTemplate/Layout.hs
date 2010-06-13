module Bolt.Plugin.XHtmlTemplate.Layout (
  -- single template var
    (#)
  -- list of template vars
  , (##)
  -- html to string .. for attributes
  , (#$)
  -- reexport from types
  , XHtmlLayout
  ) where

import Bolt.Plugin.XHtmlTemplate.Types


import qualified Data.Map as M
import qualified Text.XHtml as X
import Data.Maybe

-- helpers to grab templatevars from within vies/layouts
(#) a b = tvExtract $ fromMaybe (toTmplVar $ "") (M.lookup b a)
(##) a b = tvExtractList $ fromMaybe (toTmplVar $ ([] :: [String])) (M.lookup b a)
(#$) a b = X.showHtmlFragment $ (#) a b
