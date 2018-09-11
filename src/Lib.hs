{-# LANGUAGE OverloadedStrings #-}
-- vim: set et sw=4 ts=4:
module Lib where
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Function (on)
import Data.Map.Strict hiding (filter, map, null, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.List hiding (lookup)
import Network.HTTP
import Prelude hiding (lookup)
import System.Environment
import System.IO.Unsafe

getAPIKey :: IO String
getAPIKey = getEnv "CHAMPION_GG_API_KEY"

type ChampKey = String

req :: FromJSON o => [String] -> [(String, String)] -> IO o
req paths args = do
    api_key <- getAPIKey
    let pathStr = intercalate "/" paths
    let argStr = intercalate "&" $ map (\(a, b) -> a ++ "=" ++ b) $ ("api_key", api_key):args
    result <- simpleHTTP (getRequest $ "http://api.champion.gg/" ++ pathStr ++ "?" ++ argStr)
    body <- getResponseBody result
    ioDecode body

data DataPage = DataPage{data' :: [Object]}
instance FromJSON DataPage where parseJSON (Object o) = DataPage <$> o .: "data"
                                 parseJSON _ = error "DataPage expects an Object"

-- uses unsafeInteleaveIO to lazily retrieve pages as they are required
reqPaginated :: FromJSON o => [String] -> [(String, String)] -> IO o
reqPaginated paths args = fmap (parse . concat . takeWhile (not . null)) $ sequence $ map fetch $ zip (iterate (*2) 100) [1..3]
    where fetch (limit, page) = unsafeInterleaveIO $ fmap data' $ req paths (("limit", show limit):("page", show page):args)
          parse obj = either error id $ parseEither parseJSON $ toJSON obj

-- decode from json, raising io errors for any funkiness
ioDecode :: FromJSON a => String -> IO a
ioDecode str = either error return $ eitherDecode (pack str)

type PlayRateMap = Map ChampKey Float
type WinRateMap = Map ChampKey (Map ChampKey Float)
data State = State [ChampKey] WinRateMap PlayRateMap deriving(Show, Read, Eq)

-- gets all needed state; other functions use a Reader monad to simplify passing through of the pulled dataset
-- role is one of "Jungle", "Top", "Middle", "ADC", "Support"
pullState :: String -> IO State
pullState role = do
    (prm, champs) <- playRateAndChamps role
    wrm <- winRateMap champs role
    return $ State champs wrm prm

playRateAndChamps :: String -> IO(PlayRateMap, [ChampKey])
playRateAndChamps role = do
    allStats <- reqPaginated ["stats", "role", role] []
    let champs = map statsKey allStats
    let prm = fromList $ map (\s -> (statsKey s, playPercent s)) allStats
    return (prm, champs)


winRateMap :: [ChampKey] -> String -> IO(WinRateMap)
winRateMap champs role = do
    champMatchupMaps <- sequence $ map processChamp champs
    return $ fromList $ zip champs champMatchupMaps
    where processChamp c = do
            ms <- champMatchups c role
            return $ fromList $ map (\m -> (enemy m, statScore m)) ms

data RoleMatchups = RoleMatchups {
    matchupRole :: String,
    matchups :: [Matchup]
} deriving (Show)
instance FromJSON RoleMatchups where
    parseJSON (Object o) = RoleMatchups <$> o .: "role" <*> o .: "matchups"
    parseJSON wtf = error $ "RoleMatchups expects an Object, got " ++ show wtf

data Matchup = Matchup {
    number :: Integer,
    statScore :: Float,
    winRate :: Float,
    enemy :: ChampKey
} deriving (Show)

instance FromJSON Matchup where
    parseJSON (Object o) = Matchup <$> o .: "games" <*> o .: "statScore" <*> o .: "winRate" <*> o .: "key"
    parseJSON _ = error "Matchup expects an Object"

champMatchups :: String -> String -> IO [Matchup]
champMatchups cKey role = do
    ms <- req ["champion", cKey, "matchup"] []
    let forRole = take 1 . filter (\m -> matchupRole m == role) $ ms
    return $ if length forRole == 1 then matchups . head $ forRole else []

data Counterpick = Counterpick {
    targetEnemy :: ChampKey,
    counter :: ChampKey,
    score :: Maybe Float
}

instance Show Counterpick where
    show (Counterpick c cnt s) = show (c, cnt, s)

type WithStats = Reader State
champScore :: ChampKey -> WithStats Float
champScore c = askChamps >>= fmap (sum . catMaybes) . sequence . map (weightedScore c)

-- gives champ from your pool who is the best on average against all opponents
bestAllRounder :: [ChampKey] -> WithStats (ChampKey, Float)
bestAllRounder pool = do
    winrates <- askWinrates
    champs <- askChamps
    scores <- sequence $ map champScore pool
    return $ maximumBy (compare `on` snd) $ zip pool scores

-- takes statscore of the matchup and weighs it by the playrate of the enemy
weightedScore :: ChampKey -> ChampKey -> WithStats (Maybe Float)
weightedScore you enemy = do
    playrates <- askPlayrates
    winrates <- askWinrates
    let winrate = lookup you winrates >>= lookup enemy
    let playrate = playrates ! enemy
    return $ fmap (playrate*) winrate

-- gives a sorted list of your best counterpicks for a given champ
bestCounterpicks :: [ChampKey] -> ChampKey -> WithStats [Counterpick]
bestCounterpicks pool champ = do
    winrates <- askWinrates
    let buildCounter c = Counterpick champ c (lookup c winrates >>= lookup champ)
    return $ sortBy (\a b -> compare (score b) (score a)) $ map buildCounter pool

-- gives every champion and your best counterpick for it, given your pool
allBestCounterpicks :: [ChampKey] -> WithStats [Counterpick]
allBestCounterpicks pool = askChamps >>= sequence . map (\c -> fmap head $ bestCounterpicks pool c)

--gives the aggregate statscore for your pool
poolScore :: [ChampKey] -> WithStats (Float, (ChampKey, Float), [(ChampKey, ChampKey)])
poolScore pool = do
    (State champs _ playrates) <- ask
    counterpicks <- allBestCounterpicks pool
    let statscores = catMaybes $ map (\c -> score c >>= return.(* playrates ! targetEnemy c)) counterpicks
    let average = sum statscores / fromIntegral (length statscores)
    allRounder <- bestAllRounder pool
    let score = (0.5 * snd allRounder) + (0.5 * average)
    let pickPairs = zip (map counter counterpicks) $ map targetEnemy counterpicks
    return (score, allRounder, pickPairs)

--given your champ pool, provides a sorted list of best single improvements to it
bestImprovement :: [ChampKey] -> WithStats [(ChampKey, Float)]
bestImprovement pool = do
    champs <- askChamps
    current <- statscore pool
    let candidates = champs \\ pool
    scores <- sequence $ map (\c -> statscore (pool ++ [c])) candidates
    return $ sortBy (flip compare `on` snd) $ ("Unchanged", current):zip candidates scores

renderStuff :: Show a => [a] -> IO()
renderStuff xs = putStr $ unlines $ map show xs

renderImprovements :: [(ChampKey, Float)] -> String
renderImprovements cs = unlines $ map (\(c, s) -> c ++ plot c s) cs
    where unchanged = snd $ last cs
          plot c s = take (1 + maxclen - length c) (repeat ' ') ++ (take (round $ (s - unchanged) * 2) $ repeat '*')
          maxclen = maximum $ map (length . fst)  cs

data ChampStats = ChampStats {
    statsKey :: String,
    playPercent :: Float
} deriving (Show)
instance FromJSON ChampStats where
    parseJSON (Object o) = ChampStats <$>
        o .: "key" <*>
        (o .: "general" >>= (.: "playPercent"))
    parseJSON wtf = error $ "ChampStats expects an Object, got " ++ show wtf

statscore :: [ChampKey] -> WithStats Float
statscore [] = return 0
statscore pool = do
    (score, _, _) <- poolScore pool
    return score

askChamps :: WithStats [ChampKey]
askChamps = ask >>= \(State x _ _) -> return x

askWinrates :: WithStats WinRateMap
askWinrates = ask >>= \(State _ x _) -> return x

askPlayrates :: WithStats PlayRateMap
askPlayrates = ask >>= \(State _ _ x) -> return x

wilsonConfidenceInterval :: Float -> Int -> (Float, Float)
wilsonConfidenceInterval winrate games = (p1 - p2, p1 + p2)
    where z = 1.96 -- hardcoded 95%
          p = winrate
          n = fromIntegral games
          p1 = (p + ((z*z)/(2*n)))
          p2 = (z * (sqrt $ ((p*(1-p)) + (z*z)/(4*n))/n))/((1+((z*z)/n)))
