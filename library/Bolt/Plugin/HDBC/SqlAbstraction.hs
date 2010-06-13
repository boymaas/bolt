{-# LANGUAGE FlexibleContexts #-}
-- | Defines and implements shared Sql functionality
module Bolt.Plugin.HDBC.SqlAbstraction
       where
import Database.HDBC

import qualified Data.List as DL
import Data.Convertible

-- Simple sql abstraction
-- to be used by the HDBC implmentations. Giving
-- them similar interfaces. Making swapping between
-- different types of databases almost trivial.
 
type Table = String
 
type Criteria = String
 
type Values = [SqlValue]
 
type RowsAffected = Integer
 
type ColumnName = String
 
type ColumnValuePair = (ColumnName, SqlValue)
 
type Query = (String, [SqlValue])
type InsertStatement = (String, [SqlValue], Table)
 
data UpsertColumn = UC ColumnValuePair
                  | UCId ColumnValuePair
for = flip map
 
-- | helper to convert to Sql
tsql ::  (Convertible a SqlValue) => a -> SqlValue
tsql a = toSql a

-- | helper to convert from Sql
fsql ::  (Convertible SqlValue a) => SqlValue -> a
fsql a = fromSql a

-- | helper to generate column value pair
cvp ::  (Convertible a SqlValue) => t -> a -> (t, SqlValue)
cvp a b = (a,toSql b)

-- | runs sql over defined connection
runSql :: (IConnection conn) => conn -> Query -> IO Integer
runSql conn (query, values) = run conn query values
 
-- | runs query over defined connection / strict
query' :: (IConnection conn) => conn -> Query -> IO [[SqlValue]]
query' conn (query, values) = quickQuery' conn query values
 
-- | runs query over defined connection / lazy
query :: (IConnection conn) => conn -> Query -> IO [[SqlValue]]
query conn (query, values) = quickQuery conn query values


-- | checks if row with criteria exists
exists :: (IConnection a) => a -> Table -> Criteria -> Values -> IO Bool
exists conn table criteria values
  = do r <- quickQuery' conn query values
       case r of
           [[x]] -> return True
           _ -> return False
  where query = "select * from " ++ table ++ " where " ++ criteria
 
-- | selects all defined columns from table
select :: Table -> [ColumnName] -> Query
select table columnnames = (query, [])
  where query = "select " ++ join_comma columnnames ++ " from " ++ table

-- | select defined column from table with criteria
select_where :: Table -> [ColumnName] -> String -> [SqlValue] -> Query
select_where table cols criteria values = (sql, values)
  where sql = fst (select table cols) ++ " where " ++ criteria
 
-- | inserts columnvaluepairs into database
insert :: Table -> [ColumnValuePair] -> InsertStatement
insert table cvpairs
  = ("insert into " ++ table ++ " (" ++ columns ++ ") values (" ++ qmarks ++ ")",
     values, table)
  where columns = join_comma $ map fst cvpairs
        qmarks = join_comma $ map (\ _ -> "?") cvpairs
        values = map snd cvpairs
 
-- | updates columnvaluepairs on table with criteria
update :: Table -> [ColumnValuePair] -> Criteria -> Values -> Query
update table cvpairs criteria criteriavalues
  = ("update " ++ table ++ " set " ++ column_eq_values ++ " where " ++ criteria,
     values)
  where column_eq_values = join_comma $ map (\ (c, _) -> c ++ " = ?") cvpairs
        values = map snd cvpairs ++ criteriavalues
 
-- | define upsertcolumn only containing value
uc ::  (Convertible a SqlValue) => ColumnName -> a -> UpsertColumn
uc colname value = UC (colname, toSql value)

-- | define upsertcolumn containing value and to be used as criteria
ucid ::  (Convertible a SqlValue) => ColumnName -> a -> UpsertColumn
ucid colname value = UCId (colname, toSql value)

-- | Either update or insert row inside table 
-- based on supplied criteria
-- example:
--   upsert "tags" [ucid "exampletag", uc date]
--
-- which uses all ucid elements to build required criteria
-- returns updated row
upsert :: (IConnection conn) =>conn-> (conn -> InsertStatement -> IO Integer)-> Table-> [UpsertColumn]-> IO Integer
upsert conn runInsert table upsertcolumns
  = do updated <- runSql conn $ update table cvpairs criteria values
       case updated of
           0 -> runInsert conn $ insert table cvpairs
           _ -> return $ (-1) * updated
  where (criteria, values) = buildCriteria upsertcolumns
        cvpairs = extract_columnvaluepairs upsertcolumns

-- | builds criteria out of list of upsert columns
buildCriteria :: [UpsertColumn] -> (Criteria, Values)
buildCriteria upsertcols = (criteria, values)
  where cvpairs'
          = extract_columnvaluepairs $ filter is_an_upsertcriteria upsertcols
        criteria = join_and $ flip map cvpairs' $ \ (c, _) -> c ++ " = ?"
        values = flip map cvpairs' $ \ (_, v) -> v
        is_an_upsertcriteria cvp
          = case cvp of
                UCId cvp -> True
                otherwise -> False

extract_columnvaluepairs
  = map $
      \ cvp ->
        case cvp of
            UC cvp' -> cvp'
            UCId cvp' -> cvp'

join_comma = concat . DL.intersperse ", "
join_and = concat . DL.intersperse " AND "
