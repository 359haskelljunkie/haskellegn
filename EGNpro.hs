{-# LANGUAGE DeriveGeneric #-}

module EGNPro
  ( EGN(..)
  , Sex(..)
  , Region(..)
  , EgnData(..)
  , egnValid
  , egnParse
  , egnInfo
  , egnGenerate
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (find)
import GHC.Generics (Generic)
-- “Randomness is just IO pretending to be pure.”
import System.Random (randomRIO) 



-- =============================
-- Types
-- =============================

newtype EGN = EGN String deriving (Show, Eq)

data Sex = Male | Female deriving (Show, Eq)

data Region = Region
  { regionName :: String
  , regionLast :: Int
  } deriving (Show, Eq)


data EgnData = EgnData
  { egnYear        :: Int
  , egnMonth       :: Int
  , egnDay         :: Int
  , egnSex         :: Sex
  , egnRegion      :: Region
  , egnBirthNumber :: Int
  } deriving (Show, Eq, Generic)

-- =============================
-- Constants
-- =============================

regions :: [Region]
regions =
  [ Region "Благоевград" 43
  , Region "Бургас" 93
  , Region "Варна" 139
  , Region "Велико Търново" 169
  , Region "Видин" 183
  , Region "Враца" 217
  , Region "Габрово" 233
  , Region "Кърджали" 281
  , Region "Кюстендил" 301
  , Region "Ловеч" 319
  , Region "Монтана" 341
  , Region "Пазарджик" 377
  , Region "Перник" 395
  , Region "Плевен" 435
  , Region "Пловдив" 501
  , Region "Разград" 527
  , Region "Русе" 555
  , Region "Силистра" 575
  , Region "Сливен" 601
  , Region "Смолян" 623
  , Region "София - град" 721
  , Region "София - окръг" 751
  , Region "Стара Загора" 789
  , Region "Добрич" 821
  , Region "Търговище" 843
  , Region "Хасково" 871
  , Region "Шумен" 903
  , Region "Ямбол" 925
  , Region "Друг/Неизвестен" 999
  ]

weights :: [Int]
weights = [2,4,8,5,10,9,7,3,6]

-- =============================
-- Helpers
-- =============================

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m `elem` [1,3,5,7,8,10,12] = 31
  | m `elem` [4,6,9,11]        = 30
  | m == 2                     = if isLeap y then 29 else 28
  | otherwise                  = 0

checkDate :: Int -> Int -> Int -> Bool
checkDate m d y = m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth m y

chunks :: String -> Maybe [Int]
chunks s
  | length s == 10 && all isDigit s = Just (map digitToInt s)
  | otherwise = Nothing

checksumOK :: [Int] -> Bool
checksumOK ds = last ds == valid
  where
    s = sum (zipWith (*) (take 9 ds) weights)
    r = s `mod` 11
    valid = if r == 10 then 0 else r

-- =============================
-- Core
-- =============================

egnValid :: String -> Bool
egnValid s = case chunks s of
  Nothing -> False
  Just ds -> validDate ds && checksumOK ds

validDate :: [Int] -> Bool
validDate ds = checkDate m d y
  where
    yr  = ds!!0 * 10 + ds!!1
    mon = ds!!2 * 10 + ds!!3
    d   = ds!!4 * 10 + ds!!5

    (y,m)
      | mon > 40  = (yr + 2000, mon - 40)
      | mon > 20  = (yr + 1800, mon - 20)
      | otherwise = (yr + 1900, mon)

-- =============================
-- Parsing
-- =============================

egnParse :: String -> Either String EgnData
egnParse s = case chunks s of
  Nothing -> Left "Невалиден формат"
  Just ds
    | not (validDate ds) -> Left "Невалидна дата"
    | not (checksumOK ds) -> Left "Невалидна контролна цифра"
    | otherwise -> Right (build ds)

build :: [Int] -> EgnData
build ds =
  EgnData y m d sex region birthN
  where
    yr  = ds!!0 * 10 + ds!!1
    mon = ds!!2 * 10 + ds!!3
    d   = ds!!4 * 10 + ds!!5
    code = ds!!6 * 100 + ds!!7 * 10 + ds!!8

    (y,m)
      | mon > 40  = (yr + 2000, mon - 40)
      | mon > 20  = (yr + 1800, mon - 20)
      | otherwise = (yr + 1900, mon)

    sex = if even (ds!!8) then Male else Female

    region = findRegion code
    first = regionFirst region

    adj = if odd code then code - 1 else code
    birthN = (adj - first) `div` 2 + 1

findRegion :: Int -> Region
findRegion code =
  maybe (last regions) id (find (\r -> code <= regionLast r) regions)

regionFirst :: Region -> Int
regionFirst r =
  case findPrev r regions 0 of
    Just x -> x
    Nothing -> 0

findPrev :: Region -> [Region] -> Int -> Maybe Int
findPrev _ [] _ = Nothing
findPrev target (r:rs) acc
  | r == target = Just acc
  | otherwise = findPrev target rs (regionLast r + 1)

-- =============================
-- Info
-- =============================

egnInfo :: String -> String
egnInfo s = case egnParse s of
  Left err -> s ++ " — " ++ err
  Right d  ->
    "ЕГН принадлежи на " ++ show (egnSex d)
    ++ ", роден на " ++ show (egnDay d) ++ "." ++ show (egnMonth d)
    ++ "." ++ show (egnYear d)
    ++ ", регион " ++ regionName (egnRegion d)

-- =============================
-- Generation
-- =============================

egnGenerate :: IO String
egnGenerate = do
  y <- randomRIO (1900, 2099)
  m <- randomRIO (1,12)
  d <- randomValidDay m y
  reg <- randomRIO (0,999)

  let base = pad2 (y `mod` 100) ++ pad2 (adjustMonth y m) ++ pad2 d ++ pad3 reg
      ck = calcChecksum base

  return (base ++ show ck)

randomValidDay :: Int -> Int -> IO Int
randomValidDay m y = randomRIO (1, daysInMonth m y)

adjustMonth :: Int -> Int -> Int
adjustMonth y m
  | y < 1900 = m + 20
  | y >= 2000 = m + 40
  | otherwise = m

calcChecksum :: String -> Int
calcChecksum base =
  let ds = map digitToInt base
      s = sum (zipWith (*) ds weights)
      r = s `mod` 11
  in if r == 10 then 0 else r

pad2 :: Int -> String
pad2 n | n < 10 = '0':show n
       | otherwise = show n

pad3 :: Int -> String
pad3 n
  | n < 10 = "00" ++ show n
  | n < 100 = "0" ++ show n
  | otherwise = show n
