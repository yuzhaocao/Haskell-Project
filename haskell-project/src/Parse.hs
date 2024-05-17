{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseMonsters,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8


-- |Inorder to avoid any conflicts due to the wording "type" we changed it to "monstertype".
renameFields :: String -> String
renameFields "monstertype" = "type"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

-- |Generates a JSON representation of Haskell data and writes to a file
instance FromJSON Monsterone where
    parseJSON = genericParseJSON customOptions

instance FromJSON Monster



parseMonsters :: L8.ByteString -> Either String Monster
parseMonsters j = eitherDecode j :: Either String Monster