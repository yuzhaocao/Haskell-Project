module Main (main) where

import System.IO
import Fetch
import Parse
import Types
import Database

-- |Interactive design with options
main :: IO ()
main = do
    putStrLn "-----------------------------------------------------"
    putStrLn "  Welcome to Dungeons & Dragons 5e monster book      "
    putStrLn "  (1) Download data                                  "
    putStrLn "  (2) All monsters by strength(str) value            " 
    putStrLn "  (3) total monsters by strength(str) value          " 
    putStrLn "  (4) Monsters with same alignment                   "
    putStrLn "  (5) All monsters by size                           "
    putStrLn "  (6) Quit                                           "
    putStrLn "-----------------------------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://raw.githubusercontent.com/mdogdope/DND-5E-Data/main/monsters.json"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseMonsters json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveMonsters conn (monster recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryStrAllEntries conn
            mapM_ print entries
            main
        3 -> do
            queryStrTotalEntries conn
            main
        4 -> do
            entries <- queryAlignmentAllEntries conn
            mapM_ print entries
            main
        5 -> do 
            entries <- querySizeAllEntries conn
            mapM_ print entries
            main
        6 -> print "Hope you've enjoyed using the Dungeons & Dragons 5e monster book!"
        _ -> print "Invalid option"

