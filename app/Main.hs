{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (rights)
import Data.List (groupBy, nub, nubBy, sortBy)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Fmt
import Fmt.Internal
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (stdout)
import Text.Read (readEither)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T


-- Data type for the information from the JSON file
data MediaInfo = MediaInfo
    { rechtstraeger       :: Payer
    , quartal             :: Quartal
    , bekanntgabe         :: Int
    , mediumMedieninhaber :: Recipient
    , euro                :: Double
    } deriving (Show, Eq, Generic)

type Payer = Text
type Recipient = Text
type Quartal = Text

data Top = Top
    { tPayer     :: Payer
    , tRecipient :: Recipient
    , tEuro      :: Double
    } deriving (Show, Eq)

emptyTop :: Top
emptyTop = Top
    { tPayer     = T.empty
    , tRecipient = T.empty
    , tEuro      = 0
    }

data Detail = Detail
    { dAgent     :: Text
    , dEuro      :: Double
    , dCategory  :: Int
    } deriving (Show, Eq)

emptyDetail :: Detail
emptyDetail = Detail
    { dAgent    = T.empty
    , dEuro      = 0
    , dCategory  = 0
    }

-- Instances to convert our type to/from JSON.
-- It is enough for simple types.
instance FromJSON MediaInfo

media :: Value -> Either String [MediaInfo]
media = parseEither $ withObject "people" $ \o -> o .: "data"

data QuartalOverview = QuartalOverview
    { qoQuartal       :: Quartal
    , qoBekanntgabe2  :: Double
    , qoBekanntgabe4  :: Double
    , qoBekanntgabe31 :: Double
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
            "help" -> fmt helpText
            "exit" -> exitSuccess
            -- Set 2
            "payers" -> do
                ps <- readMVar mi
                showPayers ps
            "recipients" -> do
                ps <- readMVar mi
                showRecipients ps
            -- Set 3
            "quarters" -> do
                ps <- readMVar mi
                mapM_ (\QuartalOverview{..} -> fmtLn
                    $ (build qoQuartal)
                    <> (padLeftF 24 ' ' $ formatNum qoBekanntgabe2 <> "(§2),")
                    <> (padLeftF 24 ' ' $ formatNum qoBekanntgabe4 <> "(§4),")
                    <> (padLeftF 24 ' ' $ formatNum qoBekanntgabe31 <> "(§31)"))
                    $ overviewByQuartal ps
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
sortByFieldDesc :: Ord a => (t -> a) -> [t] -> [t]
sortByFieldDesc field = sortBy (\a b -> flip compare (field a) (field b))

groupByFieldDesc :: Ord a => (t -> a) -> [t] -> [[t]]
groupByFieldDesc field = groupBy (\a b -> field a == field b)

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

showRecipients :: [MediaInfo] -> IO ()
showRecipients ps = do
    mapM_ T.putStrLn $ filterRecipients ps

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
parseQuartalArgs [] = [lastQuartal]
parseQuartalArgs args@(x:xs)
    | null xs   = either (const [lastQuartal]) checkOne (readEither x :: Either String Int)
    | otherwise = if null check then [lastQuartal] else check
  where
    rs = rights (map readEither (nub args) :: [Either String Int])
    check = catMaybes $ map checkMany rs
    checkNumber n = let (year, quart) = divMod n 10
                    in and [1990 <= year, year <= 2100, 0 < quart, quart < 5]
    checkMany n = if checkNumber n then Just n else Nothing
    checkOne n = if checkNumber n then [n] else [lastQuartal]

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
        (\Top{..} -> fmtLn $ padRightF 65 ' ' (build tPayer) <> padBothF 1 ' ' (":" :: Text) <> padLeftF 15 ' '  (formatNum tEuro))
        (sortByEuro $ reduceAgents $ groupAgents rechtstraeger)
    "recipients" -> mapM_
        (\Top{..} -> fmtLn $ padRightF 65 ' ' (build tRecipient) <> padBothF 1 ' ' (":" :: Text) <> padLeftF 15 ' '  (formatNum tEuro))
        (sortByEuro $ reduceAgents $ groupAgents mediumMedieninhaber)
    _ -> pure ()
  where
    sortByEuro :: [Top] -> [Top]
    sortByEuro = take n . sortByFieldDesc tEuro
    reduceAgents :: [[MediaInfo]] -> [Top]
    reduceAgents = map (foldr sumTopEuro emptyTop)
    groupAgents :: (MediaInfo -> Payer) -> [[MediaInfo]]
    groupAgents agent = groupByFieldDesc agent $ sortByFieldDesc agent filterByCat
    filterByCat :: [MediaInfo]
    filterByCat = filter (\MediaInfo{..} -> cat == bekanntgabe) mi

sumTopEuro :: MediaInfo -> Top -> Top
sumTopEuro MediaInfo{..} Top{..} = Top
    { tPayer = rechtstraeger
    , tRecipient = mediumMedieninhaber
    , tEuro = tEuro + euro
    }

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
    "payers"     -> Right "payers"
    "recipients" -> Right "recipients"
    _            -> Left "Wrong second argument. 'payers' or 'recipients' are awaited."

readCategory :: String -> Either String Int
readCategory cat = case cat of
    "§2"  -> Right 2
    "§4"  -> Right 4
    "§31" -> Right 31
    _    -> Left "Wrong category. 2,4,31 are awaited."

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
details :: Text -> Text -> [MediaInfo] -> [Detail]
details who name ps = sortByFieldDesc dEuro foundByName
  where
    foundByName = case who of
        "payers" -> listAgents rechtstraeger mediumMedieninhaber
        "recipients" -> listAgents mediumMedieninhaber rechtstraeger
        _ -> error "Wrong first argument. 'payers' or 'recipients' are awaited."

    listAgents :: (MediaInfo -> Text) -> (MediaInfo -> Text) -> [Detail]
    listAgents who' name' = concatMap (reduceAgents who') $ map (groupAgents who') $ groupAgents bekanntgabe $ filterByAgent name' name
    reduceAgents :: (MediaInfo -> Text) -> [[MediaInfo]] -> [Detail]
    reduceAgents agent = map (foldr (sumDetailEuro agent) emptyDetail)
    groupAgents agent = groupByFieldDesc agent . sortByFieldDesc agent

    filterByAgent :: (MediaInfo -> Text) -> Text -> [MediaInfo]
    filterByAgent agent name' = filter (\mi -> T.toLower name' == T.toLower (agent mi)) ps

sumDetailEuro :: (MediaInfo -> Text) -> MediaInfo -> Detail -> Detail
sumDetailEuro agent mi@MediaInfo{..} Detail{..} = Detail
    { dAgent = agent mi
    , dEuro = dEuro + euro
    , dCategory = bekanntgabe
    }

showDetails :: [Detail] -> IO ()
showDetails mi = do
    writeFile "top.txt" $ show mi
    let cat2 = filter (\Detail{..} -> 2 == dCategory) mi
    let cat4 = filter (\Detail{..} -> 4 == dCategory) mi
    let cat31 = filter (\Detail{..} -> 31 == dCategory) mi
    putStrLn "Payments according to §2:"
    pprintDetails cat2
    putStrLn "Payments according to §4:"
    pprintDetails cat4
    putStrLn "Payments according to §31:"
    pprintDetails cat31

pprintDetails :: [Detail] -> IO ()
pprintDetails ds =
    mapM_ (\Detail{..} ->
        fmtLn $ padRightF 70 ' ' (build dAgent) <> padLeftF 15 ' ' (formatNum dEuro)) ds

-- Other
jsonURL :: Int -> String
jsonURL yearQuart = concat
    [ "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal="
    , show yearQuart
    , "&leermeldung=0&size=0"
    ]

helpText :: Builder
helpText = unlinesF
    [ "'help'" <> dots <> "print this message"
    , "'exit'" <> dots <> "terminate this application"
    , "'payers'" <> dots <> "print a list of all payers"
    , "'recipients'" <> dots <> "print a list of all recipient"
    , "'quarters'" <> dots <> "print total sum for every payment category"
    , "'top' n 'payers'|'recipients' '§2'|'§4'|'§31'" <> dots <> "print the n biggest payers|recipients for the given payment type"
    , "'search' 'payers'|'recipients' searchTerm" <> dots <> "print a list of all payers|recipients containing the given searchTerm"
    , "'load' quarter1 quarter2 .. quarterN" <> dots <> "load data for the given list of quarters"
    , "'details' 'payers'|'recipients' organization" <> dots <> "print a list of all payments payed or received by the given payers/recipient"
    ]
  where
    dots = padBothF 5 ' ' ("..." :: Text)

lastQuartal :: Int
lastQuartal = 20184

-- Formating functions

-- Get integer part of number and add dots between thousands
formatInteger :: Double -> Builder
formatInteger d = groupInt 3 '.' (floor d :: Integer)

-- Get 2 digits of decimal part of number
formatDecimal :: Double -> Builder
formatDecimal = suffixF 2 . fixedF 2

-- Build formated number. Aka "i.iii.iii,dd", "1.000,34"
formatNum :: Double -> Builder
formatNum n = formatInteger n <> "," <> formatDecimal n
