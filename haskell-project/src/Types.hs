{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
    Strtable_ (..),
    Monsterone (..),
    Monster (..)
) where

import GHC.Generics

-- |Create data types for the table entries
data Entry = Entry {
    alignment_ :: String,
    ac_ :: String,
    hp_ :: String,
    fk_str :: Int
} deriving (Show)

-- |Create data types for the table strs
data Strtable_ = Strtable_ {
    id_ :: Int,
    name_ :: String,
    size_ :: String,
    monstertype_ :: String,
    str_ :: String
} deriving (Show)

-- |Create data types to use every field from the dataset in order to pass into tables given above.
data Monsterone = Monsterone {
    name :: String,
    size :: String,
    monstertype :: String,
    alignment :: String,
    ac :: String,
    hp :: String,
    str :: String
} deriving (Show, Generic)

-- |Declare the data types for every entry present in the dataset
data Monster = Monster {
    monster :: [Monsterone]
} deriving (Show, Generic)




