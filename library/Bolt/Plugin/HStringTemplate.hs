module Bolt.Plugin.HStringTemplate (
    plugin
  , pHstRender
  , pHstRenderTmpl
  , pHstRenderTmplAttr
  , pHstRenderPrimHtml
  , pHstTmpl
  , pHstAttr
  , pHstAttrMany
  , tv) where

import Bolt.Plugin.Base

import Network.CGI
import Network.CGI.Protocol (CGIResult(..))
import Text.StringTemplate

import Text.StringTemplate.Classes
import Text.StringTemplate.Base
import Text.XHtml.Strict (primHtml)

import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe

thisPluginName = "HStringTemplate"

plugin ::  Bool -> String -> PbPluginDefinition
plugin devmode dir
  = emptyPlugin {
        pbPdName=thisPluginName
      
, pbPdHookInitialize=_initialize
      
, pbPdHookPostRequest=Just _renderTemplate
      
, pbPdConfig=pcEmptyPcHStringTemplate { pcHstTmplDir = dir }
    }

_initialize :: BsPluginInitializer
_initialize pconfig = do
  -- TODO: use devmode
  templateGroup <- unsafeVolatileDirectoryGroup (pcHstTmplDir pconfig) 5 :: IO (STGroup String)
  return $ pconfig { pcHstTmplGroup = Just templateGroup }

_renderTemplate ::  BtBolt ()
_renderTemplate = do 
  ho <- bshHasOutput 
  case ho of 
    False -> pHstRender >>= bshOutput
    True  -> return () 

-- {{{ HStringTemplate addon
-- added functionality to HStringTemplate
-- needed to store SElem in a Map to enable 
-- setting variables before defined. This could go wrong when HStringTemplate
-- is updated. But for now is sufficient
setAttributeNoConvert :: (Stringable a) => String -> SElem a -> StringTemplate a -> StringTemplate a
setAttributeNoConvert s x st = st {senv = envInsApp s (x) (senv st)}

envInsApp :: Stringable a => String -> SElem a -> SEnv a -> SEnv a
envInsApp  s  x  y = y {smp = M.insertWith go s x (smp y)}
    where go a (LI bs) = LI (a:bs)
          go a b = LI [a,b]
-- }}}

-- functions to be used by bolt controllers
pHstTmpl tn = do 
  tg <- _confAttrJust pcHstTmplGroup
  bshConfigModify thisPluginName $ \c -> c { pcHstTmpl = getStringTemplate tn tg }

pHstAttrMany :: [(String, SElem String)] -> BtBolt ()
pHstAttrMany vals = 
  mapM_ setAttribute vals
  where 
    setAttribute (l,v) = bshConfigModify thisPluginName $ \c -> c { pcHstTmplVars = M.insert l v (pcHstTmplVars c) }

pHstAttr ::  (ToSElem a) => String -> a -> BtBolt ()
pHstAttr l v = do 
  bshConfigModify thisPluginName $ \c -> c { pcHstTmplVars = M.insert l (toSElem v) (pcHstTmplVars c) }

pHstRender = do 
  tmpl <- _confAttr pcHstTmpl
  case tmpl of
    Just tmpl' -> addAttributesAndRenderTemplate tmpl'
    Nothing -> emptyString
  where
    emptyString = return ""
    addAttributesAndRenderTemplate tmpl = do
      tmplVars <- _confAttr pcHstTmplVars
      let tmpl' = foldr setTmplAttr tmpl (M.toList tmplVars)
      return $ toString tmpl'
      where
        setTmplAttr (l,v) t = setAttributeNoConvert l v t

pHstRenderTmpl ::  String -> BtBolt String
pHstRenderTmpl t = pHstRenderTmplAttr t []

pHstRenderTmplAttr :: String -> [(String, SElem String)] -> BtBolt String
pHstRenderTmplAttr tmplName tmplVars = do
  -- fetch from group
  tg <- _confAttrJust pcHstTmplGroup
  let tmpl  = fromJust $ getStringTemplate tmplName tg 
  -- set all supplied vars
  let tmpl' = foldr setTmplAttr tmpl tmplVars
  -- render to string
  return $ toString tmpl'
  where
    setTmplAttr (l,v) t = setAttributeNoConvert l v t

-- for interfacing with XHtml
pHstRenderPrimHtml tmpl = primHtml <$> pHstRenderTmpl tmpl

-- helpers
tv ::  (ToSElem a) => String -> a -> (String, SElem String)
tv a b = (a,toSElem b)


-- conf - accessors and changers
_confAttr f = _conf >>= return . f
_confAttrJust f = _conf >>= return . fromJust . f

-- shortcuts
_conf = bshConfig thisPluginName

