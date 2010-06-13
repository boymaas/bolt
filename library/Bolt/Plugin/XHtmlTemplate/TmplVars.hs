{-# LANGUAGE FlexibleInstances #-}
module Bolt.Plugin.XHtmlTemplate.TmplVars where

import qualified Text.XHtml as X

-- Keeping things simple
-- When setting templatevars we want everything to be converted to Html types
-- since XHtml works best with them. So no difference in types
-- only TvtHtml and lists of them.

data TmplVar = TvtHtml X.Html
             | TvtList [TmplVar]
             deriving Show

class ToTmplVar a where
  toTmplVar :: a -> TmplVar

-- single char
instance ToTmplVar Char where
  toTmplVar = TvtHtml . X.stringToHtml . (:[])

-- string
instance ToTmplVar [Char] where
  toTmplVar = TvtHtml . X.stringToHtml

-- list of strings
instance ToTmplVar [[Char]] where
  toTmplVar l = TvtList $ map toTmplVar l

-- Text.Xhtml.Html
instance ToTmplVar X.Html where
  toTmplVar = TvtHtml

instance ToTmplVar TmplVar where
  toTmplVar = id

tvExtract (TvtHtml a) = a

tvExtractList ::  TmplVar -> [X.Html]
tvExtractList (TvtList l) = map tvExtract l
tvExtractList _ = []

