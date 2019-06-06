{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Text.Printf (printf)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (rights)
import Data.List (groupBy, nub, sortBy, nubBy)
import Text.Read (readEither,read)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (stdout)
import Text.Pretty.Simple
import Control.Monad.Except (ExceptT, throwError, runExceptT)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding as E


-- Data type for the information from the JSON file
data MediaInfo = MediaInfo
    { rechtstraeger       :: Text
    , quartal             :: Text
    , bekanntgabe         :: Int
    , mediumMedieninhaber :: Text
    , euro                :: Float
    } deriving (Show, Eq, Generic)

-- Instances to convert our type to/from JSON.
-- It is enough for simple types.
instance FromJSON MediaInfo

media :: Value -> Either String [MediaInfo]
media = parseEither $ withObject "people" $ \o -> o .: "data"

data QuartalOverview = QuartalOverview
    { qoQuartal       :: Text
    , qoBekanntgabe2  :: Float
    , qoBekanntgabe4  :: Float
    , qoBekanntgabe31 :: Float
    } deriving (Show, Eq)

emptyOverView :: QuartalOverview
emptyOverView = QuartalOverview
    { qoQuartal = T.empty
    , qoBekanntgabe2 = 0
    , qoBekanntgabe4 = 0
    , qoBekanntgabe31 = 0
    }

-- User's command handler
cli :: MVar [MediaInfo] -> IO ()
cli mi = forever $ do
    T.putStrLn "\nEnter your command or type 'help' for assistance"
    query <- fmap E.decodeUtf8 $ BC.hPutStr stdout "> " >> BC.getLine
    let command = words $ T.unpack query
    case command of
        (q:qs) -> case q of
            "help" -> T.putStrLn helpText
            "exit" -> exitSuccess
            -- Set 2
            "payers" -> do
                ps <- readMVar mi
                showPayers ps
            "recipients" -> do
                ps <- readMVar mi
                showRecipients ps
            -- Set 3
            "quaters" -> do
                ps <- readMVar mi
                printf "%s   %s    %s    %s\n"
                    ("Quartal" :: String)
                    ("Category 2" :: String)
                    ("Category 4"  :: String)
                    ("Category 31" :: String)
                mapM_ (\QuartalOverview{..} -> printf "%s      %.2f   %.2f     %.2f\n"
                    qoQuartal qoBekanntgabe2 qoBekanntgabe4 qoBekanntgabe31) $ overviewByQuartal ps
            -- Set 4
            "load" -> do
                let quartals = parseQuartalArgs qs
                parsed <- fetchAndParseJson quartals
                void $ swapMVar mi parsed
            -- Set 5
            "top" -> do
                eitherTop <- runExceptT $ parseTopArgs qs
                case eitherTop of
                    Left err -> putStrLn err
                    Right (n,who,cat) -> do
                        ps <- readMVar mi
                        showTop ps n who cat
            "search" -> do
                eitherSearch <- runExceptT $ parse2StringArgs qs
                case eitherSearch of
                    Left err -> putStrLn err
                    Right (who, que) -> do
                        ps <- readMVar mi
                        search (T.pack who) (T.pack que) ps
            "details" -> do
                eitherDetails <- runExceptT $ parse2StringArgs qs
                case eitherDetails of
                    Left err -> putStrLn err
                    Right (who, que) -> do
                        ps <- readMVar mi
                        let ps' = details (T.pack who) (T.pack que) ps
                        showDetails ps'
            _      -> T.putStrLn $ T.concat ["Sorry, but ", "<", query, ">", " is unknown!"]
        _ -> putStrLn "No command, no job."

readJSON :: [Int] -> IO ()
readJSON quartals = do
    mi <- newEmptyMVar
    -- Get JSON data
    parsed <- fetchAndParseJson quartals
    -- Put MediaInfo list to MVar box.
    putMVar mi parsed
    -- Start cli
    cli mi

main :: IO ()
main = do
    args <- getArgs
    readJSON $ parseQuartalArgs args

-- Common functions
toText :: Show a => a -> Text
toText = T.pack . show

unUmlaut :: Text -> Text
unUmlaut txt = case T.uncons lowTxt of
    Nothing -> lowTxt
    Just (t,ts) -> case t of
        'ä' -> 'a' `T.cons` ts
        'ö' -> 'o' `T.cons` ts
        'ü' -> 'u' `T.cons` ts
        _   -> lowTxt
  where
    lowTxt = T.toLower txt

-- Set 2
showPayers :: [MediaInfo] -> IO ()
showPayers ps = do
    mapM_ T.putStrLn $ filterPayers ps
    -- T.writeFile "payers.txt" $ T.unlines $ filterPayers ps

showRecipients :: [MediaInfo] -> IO ()
showRecipients ps = do
    mapM_ T.putStrLn $ filterRecipients ps
    -- T.writeFile "recipients.txt" $ T.unlines $ filterRecipients ps

filterPayers :: [MediaInfo] -> [Text]
filterPayers = sortBy (\a b -> compare (unUmlaut a) (unUmlaut b)) . nub . map rechtstraeger

filterRecipients :: [MediaInfo] -> [Text]
filterRecipients = sortBy (\a b -> compare (unUmlaut a) (unUmlaut b)) . nub . map mediumMedieninhaber

-- Set 3
overviewByQuartal :: [MediaInfo] -> [QuartalOverview]
overviewByQuartal = map sumQuartal . groupBy (\a b -> quartal a == quartal b)
    where
        sumQuartal = foldr sumByCategory emptyOverView
        sumByCategory MediaInfo{..} QuartalOverview{..} = case bekanntgabe of
            2 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe2 = qoBekanntgabe2 + euro, qoBekanntgabe4, qoBekanntgabe31}
            4 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe4 = qoBekanntgabe4 + euro, qoBekanntgabe2, qoBekanntgabe31}
            31 -> QuartalOverview{qoQuartal = quartal, qoBekanntgabe31 = qoBekanntgabe31 + euro, qoBekanntgabe2, qoBekanntgabe4}
            _ -> error "unknown category"

-- Set 1 and Set 4
parseQuartalArgs :: [String] -> [Int]
parseQuartalArgs args = if null args then [lastQuartal] else map read args :: [Int]

fetchAndParseJson :: [Int] -> IO [MediaInfo]
fetchAndParseJson quartals = do
    putStrLn $ "Loading data for " ++ show quartals ++ " quartal(s)"
    jsons <- mapConcurrently (\q -> fmap (\j -> (q,j)) $ simpleHttp $ jsonURL q) quartals
    mapM_ (\(q,_) -> putStrLn $ "loaded data for " ++ show q) jsons
    let parsed = concat $ rights $ map ((media =<< ) . eitherDecode . snd) jsons
    pure parsed

-- Set 5
showTop :: [MediaInfo] -> Int -> String -> Int -> IO ()
showTop mi n who cat = case who of
    "payers" -> mapM_
        (\MediaInfo{..} -> T.putStrLn $ T.concat [rechtstraeger, "   ", toText euro]) sorted
    "recipients" -> mapM_
        (\MediaInfo{..} -> T.putStrLn $ T.concat [mediumMedieninhaber, "   ", toText euro]) sorted
    _ -> pure ()
  where
    sorted = take n $ sortBy (\a b -> flip compare (euro a) (euro b)) filterByCat
    filterByCat = filter (\MediaInfo{..} -> cat == bekanntgabe) mi

parseTopArgs :: [String] -> ExceptT String IO (Int, String, Int)
parseTopArgs [] = throwError "No arguments inputed."
parseTopArgs (n:str:cat:_) = do
    number <- case readNumber n of
        Left err -> throwError err
        Right w' -> pure w'
    who <- case readWho str of
        Left err -> throwError err
        Right w' -> pure w'
    category <- case readCategory cat of
        Left err -> throwError err
        Right c' -> pure c'
    pure (number, who, category)
parseTopArgs _ = throwError "Unknown agruments."

readNumber :: String -> Either String Int
readNumber = either (const $ Left "Wrong first argument. Number is awaited.") Right . readEither

readWho :: String -> Either String String
readWho str = case str of
    "payers" -> Right "payers"
    "recipients" -> Right "recipients"
    _ -> Left "Wrong second argument. 'payers' or 'recipients' are awaited."

readCategory :: String -> Either String Int
readCategory cat = case cat of
    "2"  -> Right 2
    "4"  -> Right 4
    "31" -> Right 31
    _  -> Left "Wrong category. 2,4,31 are awaited."

-- Set 6
search :: Text -> Text -> [MediaInfo] -> IO ()
search who query ps = case who of
    "payers" -> mapM_ (\MediaInfo{..} -> T.putStrLn rechtstraeger) $
        nubBy (\a b -> rechtstraeger a == rechtstraeger b) found
    "recipients" -> mapM_ (\MediaInfo{..} -> T.putStrLn mediumMedieninhaber) $
        nubBy (\a b -> mediumMedieninhaber a == mediumMedieninhaber b) found
    _ -> pure ()
    where
        found = case who of
            "payers" -> filter (\MediaInfo{..} -> q `T.isInfixOf` (T.toLower rechtstraeger)) ps
            "recipients" -> filter (\MediaInfo{..} -> q `T.isInfixOf` (T.toLower mediumMedieninhaber)) ps
            _ -> error "Wrong first argument. 'payers' or 'recipients' are awaited."
        q = T.toLower query

parse2StringArgs :: [String] -> ExceptT String IO (String, String)
parse2StringArgs [] = throwError "No arguments inputed."
parse2StringArgs (str:query) = do
    who <- case readWho str of
        Left err -> throwError err
        Right w' -> pure w'
    pure (who, unwords query)

-- Set 7
details :: Text -> Text -> [MediaInfo] -> [MediaInfo]
details who name ps = sortEuro found
  where
    found = case who of
        "payers" -> filter (\MediaInfo{..} -> T.toLower name == T.toLower rechtstraeger) ps
        "recipients" -> filter (\MediaInfo{..} -> T.toLower name == T.toLower mediumMedieninhaber) ps
        _ -> error "Wrong first argument. 'payers' or 'recipients' are awaited."
    sortEuro = sortBy (\a b -> flip compare (euro a) (euro b))

showDetails :: [MediaInfo] -> IO ()
showDetails mi = do
    let cat2 = filter (\MediaInfo{..} -> 2 == bekanntgabe) mi
    let cat4 = filter (\MediaInfo{..} -> 4 == bekanntgabe) mi
    let cat31 = filter (\MediaInfo{..} -> 31 == bekanntgabe) mi
    putStrLn "Payments according to §2:"
    mapM_ (\MediaInfo{..} ->
        T.putStrLn $ T.intercalate ", " [mediumMedieninhaber, T.pack $ show euro, "euro"]) cat2
    putStrLn "Payments according to §4:"
    mapM_ (\MediaInfo{..} ->
        T.putStrLn $ T.intercalate ", " [mediumMedieninhaber, T.pack $ show euro, "euro"]) cat4
    putStrLn "Payments according to §31:"
    mapM_ (\MediaInfo{..} ->
        T.putStrLn $ T.intercalate ", " [mediumMedieninhaber, T.pack $ show euro, "euro"]) cat31

-- Other
jsonURL :: Int -> String
jsonURL yearQuart = concat
    [ "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal="
    , show yearQuart
    , "&leermeldung=0&size=0"
    ]

helpText :: Text
helpText = T.unlines
    [ "help                                          print this message"
    , "exit                                          terminate this application"
    , "payers                                        print a list of all payers"
    , "recipients                                    print a list of all recipient"
    , "quaters                                       print total sum for every payment category"
    , "top n 'payers'|'recipients' '2'|'4'|'31'      print the n biggest payers|recipients for the given payment type"
    , "search 'payers'|'recipients' searchTerm       print a list of all payers|recipients containing the given searchTerm"
    , "load quarter1 quarter2 .. quarterN            load data for the given list of quarters"
    , "details 'payers'|'recipients' organization    print a list of all payments payed or received by the given payers/recipient"
    ]

lastQuartal :: Int
lastQuartal = 20184
