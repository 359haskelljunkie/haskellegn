# λ HaskellJunkie — ЕГН Инструмент

> Проверка, парсване и генериране на български единни граждански номера (ЕГН)  
> Логиката е написана на **Haskell**, уеб интерфейсът е пренесен в JavaScript.

🌐 **Live демо:** [359haskelljunkie.github.io/haskellegn](https://359haskelljunkie.github.io/haskellegn)

---

## 📋 Какво е ЕГН?

Единният граждански номер е 10-цифрен идентификатор използван в България. Структурата му е:

```
Г Г М М Д Д Р Р Р К
│ │ │ │ │ │ │ │ │ └─ контролна цифра
│ │ │ │ │ │ └─┴─┴─── код на регион + пол (четно=мъж, нечетно=жена)
│ │ │ │ └─┴───────── ден на раждане
│ │ └─┴─────────────  месец (1800→+20, 2000→+40)
└─┴───────────────── последните 2 цифри на годината
```

Контролната цифра се изчислява с тегла `[2, 4, 8, 5, 10, 9, 7, 3, 6]`.

---

## 🗂 Структура на проекта

```
haskellegn/
├── index.html          # Уеб приложение (self-contained, без зависимости)
├── README.md
└── haskell/
    ├── EGN.hs          # Основна логика — валидация, парсване, генериране
    └── Main.hs         # HTTP сървър върху raw sockets (без framework)
```

---

## ✨ Функционалности

| Функция | Уеб | Haskell |
|---|---|---|
| Валидация на ЕГН | ✅ | ✅ |
| Парсване (дата, пол, регион) | ✅ | ✅ |
| Генериране по критерии | ✅ | ✅ |
| История на проверките | ✅ | — |
| HTTP REST API | — | ✅ |

---

## 🚀 Пускане на Haskell сървъра локално

### Изисквания

- GHC (Glasgow Haskell Compiler)
- Пакети: `network`, `bytestring`, `random`

```bash
# Ubuntu / Debian
sudo apt install ghc libghc-network-dev libghc-random-dev
```

### Компилиране и стартиране

```bash
git clone https://github.com/359haskelljunkie/haskellegn.git
cd haskellegn/haskell

ghc -O2 -package network -package bytestring -package random Main.hs -o egn-server

./egn-server
# EGN server started at http://localhost:8080
```

### API ендпоинти

```bash
# Проверка на ЕГН
curl "http://localhost:8080/api/check?egn=7501020018"

# Генериране — мъж, роден 1990, регион Пловдив (lastNum=501), 5 броя
curl "http://localhost:8080/api/generate?year=1990&sex=1&region=501&n=5"

# Списък с всички региони
curl "http://localhost:8080/api/regions"
```

### Примерен отговор

```json
{
  "valid": true,
  "egn": "7501020018",
  "year": 1975,
  "month": 1,
  "day": 2,
  "birthday": "2 януари 1975 г.",
  "sex": 1,
  "sexText": "жена",
  "regionText": "Благоевград",
  "regionNum": 1,
  "birthNumber": 1
}
```

---

## 🔧 Haskell имплементация — ключови части

### Валидация

```haskell
egnValid :: String -> Bool
egnValid egn
  | length egn /= 10      = False
  | not (all isDigit egn) = False
  | not dateOk            = False
  | checksum /= validCk   = False
  | otherwise             = True
  where
    digits   = map digitToInt egn
    -- ... дата проверка ...
    s        = sum (zipWith (*) (take 9 digits) weights)
    validCk  = let r = s `mod` 11 in if r == 10 then 0 else r
```

### Генериране

```haskell
egnGenerate :: Int -> Int -> Int -> Int -> Maybe Int -> IO (Maybe String)
egnGenerate day mon year sex mRegionLast = go 5
  where
    go 0 = return Nothing
    go n = do
      -- генерира случайни стойности за незададените параметри
      -- изчислява контролна цифра
      -- връща валиден ЕГН
```

---

## 🏗 Технологии

- **Haskell** — бизнес логика, HTTP сървър върху `Network.Socket`

## ⚠️ Отказ от отговорност

Инструментът проверява **математическата валидност** на ЕГН по публичния алгоритъм на ЕСГРАОН. Не потвърждава самоличност на реално лице. Предназначен е за **образователни цели**.

---

## 📄 Лиценз

MIT — използвай свободно.

---

*Проект на HaskellJunkie *
