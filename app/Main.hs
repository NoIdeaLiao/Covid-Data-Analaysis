module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Text.Printf (printf)

-- Define the data structure for HospitalRecord
data HospitalRecord = HospitalRecord
  { date           :: String -- Date as a string
  , state          :: String -- Name of the state
  , beds           :: Int    -- Total hospital beds
  , bedsCovid      :: Int    -- Beds for COVID-19 patients
  , bedsNoncrit    :: Int    -- Non-critical hospital beds
  , admittedPui    :: Int    -- Admitted suspected/probable cases
  , admittedCovid  :: Int    -- Admitted COVID-19 positive cases
  , admittedTotal  :: Int    -- Total admitted cases
  , dischargedPui  :: Int    -- Discharged suspected/probable cases
  , dischargedCovid:: Int    -- Discharged COVID-19 cases
  , dischargedTotal:: Int    -- Total discharged cases
  , hospCovid      :: Int    -- Current hospitalized COVID-19 cases
  , hospPui        :: Int    -- Current hospitalized suspected/probable cases
  , hospNoncovid   :: Int    -- Current hospitalized non-COVID cases
  } deriving (Show)

-- Parse the CSV file content into HospitalRecord objects
parseCsv :: String -> [HospitalRecord]
parseCsv content =
  let rows = lines content
      header = splitByComma $ head rows
      records = tail rows
  in mapMaybe (parseRow header . splitByComma) records

-- Parse a row into a HospitalRecord using the header for column mapping
parseRow :: [String] -> [String] -> Maybe HospitalRecord
parseRow header row =
  getField "date" >>= \d ->
  getField "state" >>= \s ->
  getField "beds" >>= \b ->
  getField "beds_covid" >>= \bc ->
  getField "beds_noncrit" >>= \bn ->
  getField "admitted_pui" >>= \ap ->
  getField "admitted_covid" >>= \ac ->
  getField "admitted_total" >>= \at ->
  getField "discharged_pui" >>= \dp ->
  getField "discharged_covid" >>= \dc ->
  getField "discharged_total" >>= \dt ->
  getField "hosp_covid" >>= \hc ->
  getField "hosp_pui" >>= \hp ->
  getField "hosp_noncovid" >>= \hn ->
  HospitalRecord d s <$> readInt b <*> readInt bc <*> readInt bn
                     <*> readInt ap <*> readInt ac <*> readInt at
                     <*> readInt dp <*> readInt dc <*> readInt dt
                     <*> readInt hc <*> readInt hp <*> readInt hn
  where
    getIndex col = elemIndex col header
    getField col = getIndex col >>= (\i -> if i < length row then Just (row !! i) else Nothing)
    readInt :: String -> Maybe Int
    readInt str = case reads str of
                    [(val, "")] -> Just val
                    _           -> Nothing

-- Manually split a string by commas
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (c:cs)
  | c == ','  = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitByComma cs

-- Parse CSV from file and handle errors
parseCsvFromFile :: FilePath -> IO (Either String [HospitalRecord])
parseCsvFromFile filePath =
  readFile filePath >>= \content ->
    let records = parseCsv content
    in return $ if null records
                then Left "Error: Failed to parse CSV data. Ensure the file format is correct."
                else Right records

-- Questions

-- Q1: Find the state with the highest total hospital beds
stateWithHighestBeds :: [HospitalRecord] -> String
stateWithHighestBeds = state . maximumBy (compare `on` beds)

-- Q2: Calculate the ratio of beds dedicated to COVID-19 to total beds
covidBedRatio :: [HospitalRecord] -> String
covidBedRatio records =
  let totalCovidBeds = sum $ map bedsCovid records
      totalBeds = sum $ map beds records
      simplifiedRatio = simplifyRatio totalCovidBeds totalBeds
  in
    if totalBeds == 0
      then "No hospital beds available."
      else "No. of beds dedicated to COVID-19 : Available hospital beds:\n" ++ show (fst simplifiedRatio) ++
           " : " ++ show (snd simplifiedRatio)

-- Function to find the GCD of two numbers
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Function to simplify a ratio by dividing both numbers by their GCD
simplifyRatio :: Int -> Int -> (Int, Int)
simplifyRatio num denom =
  let gcdVal = gcd' num denom
  in (num `div` gcdVal, denom `div` gcdVal)

-- Q3: Average admissions for each state by category (PUI, COVID, Non-COVID)
averageAdmissionsByState :: [HospitalRecord] -> [(String, (Double, Double, Double))]
averageAdmissionsByState records =
  let groupedByState = groupBy ((==) `on` state) $ sortBy (compare `on` state) records
  in map calculateAverages groupedByState
  where
    calculateAverages :: [HospitalRecord] -> (String, (Double, Double, Double))
    calculateAverages stateRecords =
      let stateName = state (head stateRecords)
          avgPui    = average $ map admittedPui stateRecords
          avgCovid  = average $ map admittedCovid stateRecords
          avgNoncovid = average $ map (\rec -> admittedTotal rec - admittedCovid rec - admittedPui rec) stateRecords
      in (stateName, (avgPui, avgCovid, avgNoncovid))

    average :: [Int] -> Double
    average xs = if null xs then 0 else fromIntegral (sum xs) / fromIntegral (length xs)

-- Main program
main :: IO ()
main = do
  result <- parseCsvFromFile "resources/hospital.csv"
  case result of
    Left err -> putStrLn err
    Right records -> do
      -- Q1
      let highestState = stateWithHighestBeds records
      putStrLn $ "\nQ1.) State with the highest total hospital beds: " ++ highestState

      -- Q2
      let ratio = covidBedRatio records
      putStrLn $ "\nQ2.) Ratio of COVID-19 beds to total beds:\n" ++ ratio

      -- Q3
      let avgAdmissions = averageAdmissionsByState records
      putStrLn "\nQ3.) Average admissions by state:"
      mapM_ (\(st, (puiAvg, covidAvg, nonCovidAvg)) ->
               putStrLn $ st ++ "| Suspected/Probable: " ++ printf "%.2f" puiAvg ++
                          ", COVID Positive: " ++ printf "%.2f" covidAvg ++
                          ", Non-COVID: " ++ printf "%.2f" nonCovidAvg)
            avgAdmissions
