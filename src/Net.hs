{-# LANGUAGE DeriveGeneric #-}

module Net where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Conduit
import Text.Pretty.Simple

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as E


-- Data type for the information from the JSON file
data MediaInfo = MediaInfo
    { rechtstraeger       :: Text
    , quartal             :: Text
    , bekanntgabe         :: Int
    , mediumMedieninhaber :: Text
    , euro                :: Float
    } deriving (Show, Eq, Generic)

jsonURL :: String
jsonURL = "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal=20184&leermeldung=0&size=0"

-- Instances to convert our type to/from JSON.
-- It is enough for simple types.
instance FromJSON MediaInfo

-- instance FromJSON MediaInfo where
--     parseJSON (Object v) =
--         MediaInfo <$> v .:  "rechtstraeger"
--                   <*> v .:  "quartal"
--                   <*> v .:  "bekanntgabe"
--                   <*> v .:  "mediumMedieninhaber"
--                   <*> v .:  "euro"
--     parseJSON _ = mzero

media :: Value -> Either String [MediaInfo]
media = parseEither $ withObject "people" $ \o -> o .: "data"

readJSON :: IO ()
readJSON = do
    -- Get JSON data
    jsonBS <- simpleHttp jsonURL
    T.writeFile "media.json" $ T.toStrict $ pStringNoColor $ T.unpack $ E.decodeUtf8 jsonBS
    -- and decode it
    case media =<< eitherDecode jsonBS of
      Left err -> putStrLn err
      Right ps -> pPrintDarkBg ps

main :: IO ()
main = readJSON
