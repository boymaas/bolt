module Bolt.Forms where

import Text.XHtml
import Text.Formlets
import Data.List

import Control.Applicative
import Control.Applicative.Error

import Bolt.Types

-- Utility functions for handling Forms inside Bolt
-- 3 cases:
-- a) no input just return html
-- b) input and fail
-- c) input and success
type HFSuccessBlock a = (a -> BtBolt ())
type HFErrorBlock = [String] -> BtBolt ()
type HFRenderBlock = Html -> BtBolt ()
type HFParamList = [(String, String)]
type HFPrefix = String
type HFForm a = Form Html BtBolt a

-- | Utility function to handle Text.Formlets inside the Bolt server. Idea is that formhandling code exists
-- most of the time out of the same pattern.
--  1. no input, just return html
--  2. input and fail
--  3. input and success
-- rendering fail and success can be specified in callbacks. Abstracting the pattern.
bfHandleForm :: HFForm a -- ^ the "Text.Formlets" constructed form
    -> HFPrefix          -- ^ prefix tobe used to generate formfield names, fe `login_'
    -> HFParamList       -- ^ parameter lists int the form of [(key,value)]
    -> HFSuccessBlock a  -- ^ function to execute on success
    -> HFErrorBlock      -- ^ function to execute on failure, takes a list errors
    -> HFRenderBlock     -- ^ function to determine how to render the form, takes an html argument specifying the form code using "Text.XHtml"
    -> BtBolt ()         -- ^ runs inside the BtBolt Monad
bfHandleForm form prefix params successBlock errorBlock renderBlock = do
  let params' = map ( \(k,v) -> (k, Left v) ) 
                  . filter (\(k,v) -> isPrefixOf prefix k ) 
                    $ params

  let (getResult,getHtml,_) = runFormState params' prefix form

  r <- getResult
  html <- getHtml

  -- extract prefix out of params
  -- when params are available run the check
  -- otherwise just return rendered html

  case r of
    Failure errors -> do
      -- no params?
      case null params' of
        -- don't display errors if no params where given
        True -> renderBlock html
        -- form failed handle errorblock
        False -> errorBlock errors >> renderBlock html

    -- all checks passed execute successBlock 
    Success result -> successBlock result

  return ()
