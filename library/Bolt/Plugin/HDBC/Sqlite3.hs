-- | Implements sql functionality for Sqlite. See "Bolt.Plugin.HDBC.PostgreSQL" for more information.
module Bolt.Plugin.HDBC.Sqlite3 (
  plugin,
  -- helper to build cvpairs
  cvp,
  tsql,
  fsql,
  -- raw functions
  pHdbcRunSql,
  pHdbcRunInsert,
  pHdbcQuery,
  pHdbcQuery',
  -- querying (lazy and strict)
  pHdbcSelect,
  pHdbcSelect',
  pHdbcSelectWhere,
  pHdbcSelectWhere',
  -- changing
  pHdbcInsert,
  pHdbcUpdate,
  pHdbcUpsert,
  -- transactions
  pHdbcCommit
) 
where

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Applicative

import Bolt.Plugin.Base

import Data.Maybe

import Control.Monad.Trans

import Bolt.Plugin.HDBC.SqlAbstraction 

thisPluginName = "HDBC.Sqlite3"

plugin dbfile
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdHookInitialize=_initialize
      , pbPdConfig=pcEmptyPcHDBCSqlite3 { pcHDbFile = Just dbfile }
    }

-- connect to database
_initialize ::  PcConfigType -> IO PcConfigType
_initialize p = do 
  conn <- connectSqlite3 $ fromJust $ pcHDbFile p
  return p { pcHSqlite3Connection = Just conn }

-- runners and inserts
pHdbcRunSql :: Query -> BtBolt Integer
pHdbcRunSql query = _withConnection $ \c ->
  runSql c query

pHdbcRunInsert :: InsertStatement -> BtBolt Integer
pHdbcRunInsert insert = _withConnection $ \c ->
  runInsert c insert

pHdbcQuery ::  Query -> BtBolt [[SqlValue]]
pHdbcQuery q = _withConnection $ \c ->
  query c q

pHdbcQuery' ::  Query -> BtBolt [[SqlValue]]
pHdbcQuery' q = _withConnection $ \c ->
  query' c q

pHdbcUpsert :: Table -> [UpsertColumn] -> BtBolt Integer
pHdbcUpsert table upsertcols = _withConnection $ \c ->
  upsert c runInsert table upsertcols

-- transaction stuff
pHdbcCommit = _withConnection $ \c ->
  Database.HDBC.commit c

-- Shortcuts
pHdbcUpdate t c w v = pHdbcRunSql $ update  t c w v
pHdbcSelect t c = pHdbcQuery  $ select t c
pHdbcSelect' t c = pHdbcQuery' $ select t c
pHdbcSelectWhere t c cr v = pHdbcQuery $ select_where t c cr v
pHdbcSelectWhere' t c cr v = pHdbcQuery' $ select_where t c cr v
pHdbcInsert t cvp = pHdbcRunInsert $ insert t cvp
-- TODO: delete

_withConnection ::  (Connection -> IO a) -> BtBolt a
_withConnection block = do
  conn <- fromJust <$> _confAttr pcHSqlite3Connection
  liftIO $ block conn >>= return
  
_confAttr = bshConfigAttr thisPluginName

-- returns the last oid
runInsert :: (IConnection conn) => conn -> InsertStatement -> IO Integer
runInsert conn (stmnt, values, table) = do
  runSql conn (stmnt, values)
  r <- query' conn ("select last_insert_rowid();",[])          
  return . fromSql . head . head $ r

