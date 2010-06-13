-- | Plugin providing PostgreSQL database activity
module Bolt.Plugin.HDBC.PostgreSQL (
  -- * plugin definition
  plugin,
  -- * helpers to build cvpairs
  cvp,
  tsql,
  fsql,
  -- * Sql
  -- ** raw functions
  pHdbcRunSql,
  pHdbcRunInsert,
  pHdbcQuery,
  pHdbcQuery',
  -- ** querying (lazy and strict)
  pHdbcSelect,
  pHdbcSelect',
  pHdbcSelectWhere,
  pHdbcSelectWhere',
  -- ** changing
  pHdbcInsert,
  pHdbcUpdate,
  pHdbcUpsert,
  -- ** transactions
  pHdbcCommit
) 
where

import Database.HDBC
import Database.HDBC.PostgreSQL

import Control.Applicative

import Bolt.Plugin.Base

import Data.Maybe

import Control.Monad.Trans

import qualified Bolt.Plugin.HDBC.SqlAbstraction as SqlA
import Bolt.Plugin.HDBC.SqlAbstraction (fsql,tsql,cvp)

thisPluginName = "HDBC.PostgreSQL"


plugin ::  String -> PbPluginDefinition
plugin connectionString
  = emptyPlugin {
        pbPdName=thisPluginName
      , pbPdHookInitialize=_initialize
      , pbPdConfig=pcEmptyPcHDBCPostgreSQL { pcHPSqlConnectionString = Just connectionString }
    }

-- connect to database
_initialize ::  PcConfigType -> IO PcConfigType
_initialize p = do 
  conn <- connectPostgreSQL $ fromJust $ pcHPSqlConnectionString p
  return p { pcHPSqlConnection = Just conn }

-- runners and inserts
pHdbcRunSql :: SqlA.Query -> BtBolt Integer
pHdbcRunSql query = _withConnection $ \c ->
  SqlA.runSql c query

pHdbcRunInsert :: SqlA.InsertStatement -> BtBolt Integer
pHdbcRunInsert insert = _withConnection $ \c ->
  runInsert c insert

pHdbcQuery ::  SqlA.Query -> BtBolt [[SqlValue]]
pHdbcQuery query = _withConnection $ \c ->
  SqlA.query c query

pHdbcQuery' ::  SqlA.Query -> BtBolt [[SqlValue]]
pHdbcQuery' query = _withConnection $ \c ->
  SqlA.query' c query

pHdbcUpsert :: SqlA.Table -> [SqlA.UpsertColumn] -> BtBolt Integer
pHdbcUpsert table upsertcols = _withConnection $ \c ->
  SqlA.upsert c runInsert table upsertcols

-- transaction stuff
pHdbcCommit ::  BtBolt ()
pHdbcCommit = _withConnection $ \c ->
  Database.HDBC.commit c

-- Shortcuts
pHdbcUpdate :: SqlA.Table-> [SqlA.ColumnValuePair]-> SqlA.Criteria-> SqlA.Values-> BtBolt Integer
pHdbcUpdate t c w v = pHdbcRunSql $ SqlA.update  t c w v

pHdbcSelect :: SqlA.Table -> [SqlA.ColumnName] -> BtBolt [[SqlValue]]
pHdbcSelect t c = pHdbcQuery  $ SqlA.select t c

pHdbcSelect' :: SqlA.Table -> [SqlA.ColumnName] -> BtBolt [[SqlValue]]
pHdbcSelect' t c = pHdbcQuery' $ SqlA.select t c

pHdbcSelectWhere :: SqlA.Table-> [SqlA.ColumnName]-> String-> [SqlValue]-> BtBolt [[SqlValue]]
pHdbcSelectWhere t c cr v = pHdbcQuery $ SqlA.select_where t c cr v

pHdbcSelectWhere' :: SqlA.Table-> [SqlA.ColumnName]-> String-> [SqlValue]-> BtBolt [[SqlValue]]
pHdbcSelectWhere' t c cr v = pHdbcQuery' $ SqlA.select_where t c cr v

pHdbcInsert :: SqlA.Table -> [SqlA.ColumnValuePair] -> BtBolt Integer
pHdbcInsert t cvp = pHdbcRunInsert $ SqlA.insert t cvp
-- TODO: delete

_withConnection ::  (Connection -> IO a) -> BtBolt a
_withConnection block = do
  conn <- fromJust <$> _confAttr pcHPSqlConnection
  liftIO $ block conn >>= return

-- returns the last oid
runInsert :: (IConnection conn) => conn -> SqlA.InsertStatement -> IO Integer
runInsert conn (stmnt, values, table) = do
  SqlA.runSql conn (stmnt, values)
  r <- SqlA.query' conn ("select currval('"++ table ++"_id_seq');",[])          
  return . fromSql . head . head $ r
  
_confAttr = bshConfigAttr thisPluginName
