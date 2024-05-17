{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateStr,
    saveMonsters,
    queryStrAllEntries,
    queryStrTotalEntries,
    queryAlignmentAllEntries,
    querySizeAllEntries
) where

import Types
import Database.SQLite.Simple

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow


-- |Initialises the Database and creates the tables required if not exist
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "monster.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS strs (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \name VARCHAR(100) NOT NULL,\
            \size VARCHAR(10) NOT NULL,\
            \monstertype VARCHAR(80) NOT NULL,\
            \str VARCHAR(10) NOT NULL\
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \alignment VARCHAR(40) NOT NULL,\
            \ac VARCHAR(40) NOT NULL,\
            \hp VARCHAR(40) NOT NULL,\
            \fk_str INTERGER\
            \)"
        return conn


-- |Making data types instances of FromRow and ToRow type classes
instance FromRow Monsterone where
    fromRow = Monsterone <$> field <*> field <*> field <*> field <*> field <*> field <*> field 

instance FromRow Strtable_ where
    fromRow = Strtable_ <$> field <*> field <*> field <*> field <*> field

instance ToRow Strtable_ where
    toRow (Strtable_ i n s mty st)
        = toRow (i, n, s, mty, st)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field 

instance ToRow Entry where
    toRow (Entry al acc hpp fk_s)
        = toRow (al, acc, hpp, fk_s)


-- |Inserts values into the table strs
getOrCreateStr :: Connection -> String -> String -> String -> String -> IO Strtable_
getOrCreateStr conn n s mty st = do
    results <- queryNamed conn "SELECT * FROM strs WHERE str=:str AND name=:name" [":str" := st, ":name" := n]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO strs (name, size, monstertype, str) VALUES (?, ?, ?, ?)" (n, s, mty, st)
        getOrCreateStr conn n s mty st

-- |Inserts values into the table entries
createMonster :: Connection -> Monsterone -> IO ()
createMonster conn monsterone = do
    c <- getOrCreateStr conn (name monsterone) (size monsterone) (monstertype monsterone) (str monsterone)
    let entry = Entry {
        alignment_ = alignment monsterone,
        ac_ = ac monsterone,
        hp_ = hp monsterone,
        fk_str = id_ c
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?)" entry

-- |Dataset is been saved
saveMonsters :: Connection -> [Monsterone] -> IO ()
saveMonsters conn = mapM_ (createMonster conn)

-- |Fetches all monsters which have the strength value specified by the user.
queryStrAllEntries :: Connection -> IO [Monsterone]
queryStrAllEntries conn = do
    putStr "Enter your favorite str value [e.g. 1-30] > "
    strName <- getLine
    putStrLn $ "Looking for strength value " ++ strName ++ " entries..."
    let sql = "SELECT name, size, monstertype, alignment, ac, hp, str FROM entries inner join strs on entries.fk_str == strs.id WHERE str=?"
    query conn sql [strName]

-- |Counts the total lenth of data that is been produces from queryStrAllEntries.
queryStrTotalEntries :: Connection -> IO ()
queryStrTotalEntries conn = do
    strEntries <- queryStrAllEntries conn
    let total =  length (map str strEntries)
    print $ "Total entries: " ++ show (total)

-- |Fetches monsters which have the same alignment that is been specified.
queryAlignmentAllEntries :: Connection -> IO [Monsterone]
queryAlignmentAllEntries conn = do
    putStr "Enter your chosen monster alignment [e.g. 'unaligned', 'any alignment', 'lawful good'] > "
    alignmentName <- getLine
    putStrLn $ "Looking for " ++ alignmentName ++ " entries..."
    let sql = "SELECT name, size, monstertype, alignment, ac, hp, str FROM entries inner join strs on entries.fk_str == strs.id WHERE alignment=?"
    query conn sql [alignmentName]

-- |Fetches monsters which have the same size that is been specified.
querySizeAllEntries :: Connection -> IO [Monsterone]
querySizeAllEntries conn = do
    putStr "Enter your chosen monster size [e.g. 'S', 'M', 'L'] > "
    sizeName <- getLine
    putStrLn $ "Looking for monster size " ++ sizeName ++ " entries..."
    let sql = "SELECT name, size, monstertype, alignment, ac, hp, str FROM entries inner join strs on entries.fk_str == strs.id WHERE size=?"
    query conn sql [sizeName]
