{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Net where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Char (toLower)
import Data.List (sortBy, nub, groupBy, find)
import GHC.Generics
import Network.HTTP.Conduit
import Text.Pretty.Simple

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text.Lazy.Encoding as E


-- Data type for the information from the JSON file
data MediaInfo = MediaInfo
    { rechtstraeger       :: Text
    , quartal             :: Text
    , bekanntgabe         :: Int
    , mediumMedieninhaber :: Text
    , euro                :: Float
    } deriving (Show, Eq, Generic)

data QuartalOverview = QuartalOverview
    { qoQuartal :: Text
    , qoBekanntgabe2 :: Float
    , qoBekanntgabe4 :: Float
    , qoBekanntgabe31 :: Float
    } deriving (Show, Eq)

emptyOverView :: QuartalOverview
emptyOverView = QuartalOverview
    { qoQuartal = T.empty
    , qoBekanntgabe2 = 0
    , qoBekanntgabe4 = 0
    , qoBekanntgabe31 = 0
    }

data Payer = Payer
    { tpPayer :: Text
    , tpEuro :: Float
    , tpCategory :: Int
    } deriving (Show, Eq)

data Recipient = Recipient
    { trRecipient :: Text
    , trEuro :: Float
    , trCategory :: Int
    } deriving (Show, Eq)

-- data ShowPayerDetails = ShowPayerDetails
--     { spdPayer :: Text
--     , spdPayments2To :: [Recipient]
--     , spdPayments4To :: [Recipient]
--     , spdPayments31To :: [Recipient]
--     } deriving (Show, Eq)

-- emptyPayersDetalis :: ShowPayerDetails
-- emptyPayersDetalis = ShowPayerDetails T.empty []

-- data ShowRecipientDetails = ShowRecipientDetails
--     { srdRecipient :: Text
--     , srdPayments2From :: [Payer]
--     , srdPayments4From :: [Payer]
--     , srdPayments31From :: [Payer]
--     } deriving (Show, Eq)

-- emptyRecipientDetails :: ShowRecipientDetails
-- emptyRecipientDetails = ShowRecipientDetails T.empty []

jsonURL :: Int -> String
jsonURL yearQuart = concat
    [ "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal="
    , show yearQuart
    , "&leermeldung=0&size=0"
    ]

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

readJSON :: Int -> IO ()
readJSON yearQuart = do
    -- Get JSON data
    jsonBS <- simpleHttp $ jsonURL yearQuart
    -- T.writeFile "media.json" $ T.toStrict $ pStringNoColor $ T.unpack $ E.decodeUtf8 jsonBS
    -- and decode it
    case media =<< eitherDecode jsonBS of
      Left err -> putStrLn err
      Right ps -> do
        -- pPrintDarkBg "OK"
        -- set 2
        let payers = sortBy (\a b -> compare (unUmlaut a) (unUmlaut b)) $ nub $ map rechtstraeger ps
        -- mapM_ T.putStrLn payers
        -- T.writeFile "payers.txt" $ T.unlines payers
        let recipients = sortBy (\a b -> compare (unUmlaut a) (unUmlaut b)) $ nub $ map mediumMedieninhaber ps
        -- mapM_ T.putStrLn recipients
        -- T.writeFile "recipients.txt" $ T.unlines recipients
        -- set 3
        let overview = map overviewByQuartal $ groupBy (\a b -> quartal a == quartal b) ps
        -- print overview
        -- writeFile "overview.txt" $ show overview

        -- set 5
        let payers = toPayers $ sortBy (\a b -> flip compare (euro a) (euro b)) ps
        -- print $ take 5 Payers
        let recipients = toRecipient $ sortBy (\a b -> flip compare (euro a) (euro b)) ps
        -- print $ take 5 Recipients

        -- set 6
        let str = "Mer"
        let searchPayer s = filter (\MediaInfo{..} -> (T.toLower s) `T.isInfixOf` (T.toLower rechtstraeger)) ps
        let searchRecipient s = filter (\MediaInfo{..} -> (T.toLower s) `T.isInfixOf` (T.toLower mediumMedieninhaber)) ps
        -- print $ searchPayer str

        -- set 7
        let name = "Österreichische Post Aktiengesellschaft"
        let detailsPayer n = sortBy (\a b -> flip compare (euro a) (euro b)) $ filter (\MediaInfo{..} -> n == rechtstraeger) ps
        let detailsRecipient n = sortBy (\a b -> flip compare (euro a) (euro b)) $ filter (\MediaInfo{..} -> n == mediumMedieninhaber) ps
        showPayerDetails $ detailsPayer name
        -- mapM_ (\MediaInfo{..} -> putStrLn $ detailsPayer name
        -- print $ detailsRecipient name


main :: IO ()
main = readJSON 20184

unUmlaut :: Text -> Text
unUmlaut txt = case T.uncons lowTxt of
    Nothing -> lowTxt
    Just (t,ts) -> case t of
        'ä' -> 'a' `T.cons` ts
        -- 'Ä' -> 'A' `T.cons` ts
        'ö' -> 'o' `T.cons` ts
        -- 'Ö' -> 'O' `T.cons` ts
        'ü' -> 'u' `T.cons` ts
        -- 'Ü' -> 'U' `T.cons` ts
        _   -> lowTxt
  where
    lowTxt = T.toLower txt

overviewByQuartal :: [MediaInfo] -> QuartalOverview
overviewByQuartal = foldr sumByCategory emptyOverView
    where
        sumByCategory MediaInfo{..} QuartalOverview{..} = case bekanntgabe of
            2 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe2 = qoBekanntgabe2 + euro, qoBekanntgabe4, qoBekanntgabe31}
            4 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe4 = qoBekanntgabe4 + euro, qoBekanntgabe2, qoBekanntgabe31}
            31 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe31 = qoBekanntgabe31 + euro, qoBekanntgabe2, qoBekanntgabe4}
            _ -> error "unknown category"

toPayers :: [MediaInfo] -> [Payer]
toPayers mi = map (\MediaInfo{..} -> Payer{tpPayer = rechtstraeger, tpEuro = euro, tpCategory = bekanntgabe}) mi

toRecipient :: [MediaInfo] -> [Recipient]
toRecipient mi = map (\MediaInfo{..} -> Recipient{trRecipient = mediumMedieninhaber, trEuro = euro, trCategory = bekanntgabe}) mi

showPayerDetails :: [MediaInfo] -> IO ()
showPayerDetails mi = do
    let cat2 = filter (\MediaInfo{..} -> 2 == bekanntgabe) mi
    let cat4 = filter (\MediaInfo{..} -> 4 == bekanntgabe) mi
    let cat31 = filter (\MediaInfo{..} -> 31 == bekanntgabe) mi
    putStrLn "\n2"
    mapM_ (\MediaInfo{..} -> T.putStrLn $ T.concat [mediumMedieninhaber, "  ", T.pack $ show euro]) cat2
    putStrLn "\n4"
    mapM_ (\MediaInfo{..} -> T.putStrLn $ T.concat [mediumMedieninhaber, "  ", T.pack $ show euro]) cat4
    putStrLn "\n31"
    mapM_ (\MediaInfo{..} -> T.putStrLn $ T.concat [mediumMedieninhaber, "  ", T.pack $ show euro]) cat31

