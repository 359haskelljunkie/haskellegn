module EGN
  ( egnValid
  , egnParse
  , egnInfo
  , egnGenerate
  , EgnData(..)
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (find)
import System.Random (randomRIO)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data EgnData = EgnData
  { egnYear        :: Int
  , egnMonth       :: Int
  , egnDay         :: Int
  , egnBirthday    :: String   -- "3 март 1990 г."
  , egnRegionNum   :: Int
  , egnSex         :: Int      -- 0 = мъж, 1 = жена
  , egnSexText     :: String
  , egnRegionText  :: String
  , egnBirthNumber :: Int
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Region table  (регион -> последен номер включително)
-- ---------------------------------------------------------------------------

regions :: [(String, Int)]
regions =
  [ ("Благоевград",       43)
  , ("Бургас",            93)
  , ("Варна",            139)
  , ("Велико Търново",   169)
  , ("Видин",            183)
  , ("Враца",            217)
  , ("Габрово",          233)
  , ("Кърджали",         281)
  , ("Кюстендил",        301)
  , ("Ловеч",            319)
  , ("Монтана",          341)
  , ("Пазарджик",        377)
  , ("Перник",           395)
  , ("Плевен",           435)
  , ("Пловдив",          501)
  , ("Разград",          527)
  , ("Русе",             555)
  , ("Силистра",         575)
  , ("Сливен",           601)
  , ("Смолян",           623)
  , ("София - град",     721)
  , ("София - окръг",    751)
  , ("Стара Загора",     789)
  , ("Добрич (Толбухин)",821)
  , ("Търговище",        843)
  , ("Хасково",          871)
  , ("Шумен",            903)
  , ("Ямбол",            925)
  , ("Друг/Неизвестен",  999)
  ]

monthsBG :: [String]
monthsBG =
  [ "януари","февруари","март","април","май","юни"
  , "юли","август","септември","октомври","ноември","декември"
  ]

weights :: [Int]
weights = [2, 4, 8, 5, 10, 9, 7, 3, 6]

-- ---------------------------------------------------------------------------
-- Date helpers
-- ---------------------------------------------------------------------------

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m `elem` [1,3,5,7,8,10,12] = 31
  | m `elem` [4,6,9,11]        = 30
  | m == 2                     = if isLeap y then 29 else 28
  | otherwise                  = 0

checkDate :: Int -> Int -> Int -> Bool
checkDate m d y =
  m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth m y

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

egnValid :: String -> Bool
egnValid egn
  | length egn /= 10        = False
  | not (all isDigit egn)   = False
  | not dateOk              = False
  | checksum /= validCk     = False
  | otherwise               = True
  where
    digits = map digitToInt egn
    yr  = digits !! 0 * 10 + digits !! 1
    mon = digits !! 2 * 10 + digits !! 3
    dy  = digits !! 4 * 10 + digits !! 5

    dateOk
      | mon > 40  = checkDate (mon - 40) dy (yr + 2000)
      | mon > 20  = checkDate (mon - 20) dy (yr + 1800)
      | otherwise = checkDate  mon        dy (yr + 1900)

    checksum = last digits
    s        = sum (zipWith (*) (take 9 digits) weights)
    validCk  = let r = s `mod` 11 in if r == 10 then 0 else r

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

findRegion :: Int -> (String, Int, Int)
findRegion code = go 0 regions
  where
    go first ((name, lastN):rest)
      | code >= first && code <= lastN = (name, first, lastN)
      | otherwise                      = go (lastN + 1) rest
    go _ [] = ("Непознат", 0, 999)

egnParse :: String -> Maybe EgnData
egnParse egn
  | not (egnValid egn) = Nothing
  | otherwise          = Just EgnData
      { egnYear        = year
      , egnMonth       = month
      , egnDay         = day
      , egnBirthday    = show day ++ " " ++ (monthsBG !! (month - 1)) ++ " " ++ show year ++ " г."
      , egnRegionNum   = regionCode
      , egnSex         = sex
      , egnSexText     = if sex == 0 then "мъж" else "жена"
      , egnRegionText  = regionName
      , egnBirthNumber = birthNum
      }
  where
    digits     = map digitToInt egn
    yr         = digits !! 0 * 10 + digits !! 1
    mon        = digits !! 2 * 10 + digits !! 3
    dy         = digits !! 4 * 10 + digits !! 5
    regionCode = digits !! 6 * 100 + digits !! 7 * 10 + digits !! 8

    (year, month)
      | mon > 40  = (yr + 2000, mon - 40)
      | mon > 20  = (yr + 1800, mon - 20)
      | otherwise = (yr + 1900, mon)

    day = dy
    sex = digits !! 8 `mod` 2    -- 0 = мъж (четно), 1 = жена (нечетно)

    (regionName, firstRegion, _) = findRegion regionCode

    adjCode = if odd regionCode then regionCode - 1 else regionCode
    birthNum = (adjCode - firstRegion) `div` 2 + 1

-- ---------------------------------------------------------------------------
-- Info text
-- ---------------------------------------------------------------------------

egnInfo :: String -> String
egnInfo egn = case egnParse egn of
  Nothing -> egn ++ " — невалиден ЕГН"
  Just d  ->
    egn ++ " е ЕГН на " ++ egnSexText d ++ ", "
    ++ "роден" ++ (if egnSex d == 1 then "а" else "") ++ " на "
    ++ egnBirthday d ++ " в регион " ++ egnRegionText d ++ " "
    ++ birthDesc d

birthDesc :: EgnData -> String
birthDesc d
  | n == 1 =
      "като е " ++ (if female then "била" else "бил") ++ " "
      ++ "първото " ++ (if female then "момиче" else "момче")
      ++ " родено в този ден и регион"
  | n > 1 =
      "като преди " ++ (if female then "нея" else "него") ++ " "
      ++ "в този ден и регион са се родили " ++ show (n - 1)
      ++ (if female then " момичета" else " момчета")
  | otherwise = ""
  where
    n      = egnBirthNumber d
    female = egnSex d == 1

-- ---------------------------------------------------------------------------
-- Generation
-- ---------------------------------------------------------------------------

-- | Генерира валидно ЕГН.
--   Подай 0 / Nothing за произволна стойност.
--   sex: 0 = произволен, 1 = мъж, 2 = жена
egnGenerate
  :: Int          -- ^ ден    (0 = произволен)
  -> Int          -- ^ месец  (0 = произволен)
  -> Int          -- ^ година (0 = произволен)
  -> Int          -- ^ пол    (0 = произволен, 1 = мъж, 2 = жена)
  -> Maybe Int    -- ^ последен номер на регион (Nothing = произволен)
  -> IO (Maybe String)
egnGenerate day mon year sex mRegion = go 5
  where
    go 0 = return Nothing
    go n = do
      gday  <- if day  > 0 then return (min day 31)  else randomRIO (1, 31)
      gmon  <- if mon  > 0 then return (min mon 12)  else randomRIO (1, 12)
      gyear <- if year > 0 then return year           else randomRIO (1900, 2010)
      if not (checkDate gmon gday gyear)
        then go (n - 1)
        else do
          let cent    = gyear - gyear `mod` 100
              adjMon  = case cent of
                          1800 -> gmon + 20
                          2000 -> gmon + 40
                          _    -> gmon
              yr2     = gyear - cent

          gregion <- case mRegion of
            Nothing -> randomRIO (0, 999)
            Just lastN ->
              let firstN = regionFirst lastN
              in  randomRIO (firstN, lastN)

          let gregion' = case sex of
                1 -> if odd  gregion then gregion     else gregion + 1
                2 -> if even gregion then gregion     else gregion - 1
                _ -> gregion

          let base = pad2 yr2 ++ pad2 adjMon ++ pad2 gday ++ pad3 gregion'
              s    = sum (zipWith (*) (map digitToInt base) weights)
              ck   = let r = s `mod` 11 in if r == 10 then 0 else r
          return (Just (base ++ show ck))

    regionFirst lastN =
      case dropWhile ((/= lastN) . snd . snd) (zip (0 : map ((+1) . snd) regions) regions) of
        ((f, _):_) -> f
        []         -> 0

pad2 :: Int -> String
pad2 n | n < 10    = "0" ++ show n
       | otherwise = show n

pad3 :: Int -> String
pad3 n | n < 10    = "00" ++ show n
       | n < 100   = "0"  ++ show n
       | otherwise = show n

-- ---------------------------------------------------------------------------
-- Demo  (запуска се с  runhaskell EGN.hs)
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Проверка ==="
  let testEgns = ["7501020018", "0101010008", "9999999999", "8001010008"]
  mapM_ (\e -> putStrLn (egnInfo e)) testEgns

  putStrLn "\n=== Генериране на 5 произволни ЕГН ==="
  mapM_ (\_ -> do
    r <- egnGenerate 0 0 0 0 Nothing
    case r of
      Nothing  -> putStrLn "(неуспешно)"
      Just egn -> putStrLn (egnInfo egn)
    ) [1..5 :: Int]

  putStrLn "\n=== Мъж, роден на 15.06.1990, регион Пловдив ==="
  r <- egnGenerate 15 6 1990 1 (Just 501)
  case r of
    Nothing  -> putStrLn "(неуспешно)"
    Just egn -> putStrLn (egnInfo egn)
