{-# LANGUAGE OverloadedStrings #-}

import System.IO (readFile)
import Data.List (maximumBy, isInfixOf, intercalate)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)

-- Simple data types for student grades
type StudentGrades = (Int, String, [Double])

-- =========================
-- FUNCTIONAL PROGRAMMING
-- Functions for grade analysis
-- =========================

-- Recursive sum (pure functional)
recursiveSum :: [Double] -> Double
recursiveSum [] = 0
recursiveSum (x:xs) = x + recursiveSum xs

-- Recursive length (pure functional)
recursiveLength :: [Double] -> Double
recursiveLength [] = 0
recursiveLength (_:xs) = 1 + recursiveLength xs

-- Pure average function using recursion
calculateAverage :: [Double] -> Double
calculateAverage xs =
  if recursiveLength xs == 0
     then 0
     else recursiveSum xs / recursiveLength xs

-- Identify distinction (average > 80)
hasDistinction :: Double -> Bool
hasDistinction avg = avg > 80

-- =========================
-- JSON PARSING
-- =========================

-- Find substring position
findSubstring :: String -> String -> Int
findSubstring needle haystack = findSub 0
  where
    findSub pos
      | pos + length needle > length haystack = -1
      | take (length needle) (drop pos haystack) == needle = pos
      | otherwise = findSub (pos + 1)

-- Drop until substring is found
dropUntil :: String -> String -> String
dropUntil needle haystack =
  let pos = findSubstring needle haystack
  in if pos == -1 then "" else drop pos haystack

-- Extract value after a key in JSON
extractValue :: String -> String -> String
extractValue key json =
  let keyStr = "\"" ++ key ++ "\":"
      afterKey = dropUntil keyStr json
      afterColon = if null afterKey then "" else dropWhile (/= ':') afterKey
      trimmed = if null afterColon then "" else dropWhile isSpace (tail afterColon)
  in case trimmed of
    ('"':rest) -> takeWhile (/= '"') rest
    (c:_) | isDigit c || c == '-' -> 
      takeWhile (\x -> isDigit x || x == '.') trimmed
    _ -> ""

-- Extract all grade values from grades object
extractGrades :: String -> [Double]
extractGrades gradesStr =
  let pairs = splitOn "," gradesStr
      values = map getGradeValue pairs
  in filter (>= 0) values
  where
    getGradeValue pair =
      let afterColon = dropWhile (/= ':') pair
          numStr = dropWhile (not . isDigit) afterColon
      in case readMaybe (takeWhile (\c -> isDigit c || c == '.') numStr) :: Maybe Double of
        Just val -> val
        Nothing -> -1

-- Split string by delimiter
splitOn :: String -> String -> [String]
splitOn delim str = splitOn' delim str []
  where
    splitOn' _ [] acc = reverse acc
    splitOn' d s acc =
      let (before, after) = break (== head d) s
      in case after of
        [] -> reverse (before : acc)
        _ -> splitOn' d (drop 1 after) (before : acc)

-- Extract student ID
extractStudentId :: String -> Maybe Int
extractStudentId json = readMaybe $ extractValue "id" json

-- Extract student name
extractStudentName :: String -> String
extractStudentName json = extractValue "name" json

-- Extract grades from JSON student object
extractStudentGrades :: String -> [Double]
extractStudentGrades json =
  let gradesStart = dropUntil "\"grades\"" json
      afterGrades = if null gradesStart then "" else dropWhile (/= '{') gradesStart
      gradesObj = if null afterGrades then "" else takeWhile (/= '}') (tail afterGrades)
  in extractGrades gradesObj

-- Parse all students from JSON database
parseStudentsFromJSON :: String -> [StudentGrades]
parseStudentsFromJSON json =
  let studentsStart = dropUntil "\"students\"" json
      afterArray = if null studentsStart then "" else dropWhile (/= '[') studentsStart
      studentObjects = extractStudentObjects (if null afterArray then "" else tail afterArray) []
  in filter (\(id, _, _) -> id /= -1) $ map parseStudentObject studentObjects
  where
    extractStudentObjects [] acc = reverse acc
    extractStudentObjects s acc =
      case dropWhile (/= '{') s of
        [] -> reverse acc
        rest ->
          let obj = takeWhile (/= '}') rest
              remaining = drop 1 $ dropWhile (/= '}') rest
          in extractStudentObjects remaining (obj : acc)

    parseStudentObject obj =
      case extractStudentId obj of
        Just sid -> (sid, extractStudentName obj, extractStudentGrades obj)
        Nothing -> (-1, "", [])

-- =========================
-- RESULT PROCESSING
-- =========================

-- Create result string (simple output)
formatStudentResult :: Int -> String -> Double -> Bool -> String
formatStudentResult sid sname avg dist =
  "    { \"student_id\": " ++ show sid ++ 
  ", \"student_name\": \"" ++ sname ++ 
  "\", \"average_grade\": " ++ formatDouble avg ++ 
  ", \"has_distinction\": " ++ (if dist then "true" else "false") ++ " }"

formatDouble :: Double -> String
formatDouble d = take 5 (show d)

-- Process a list of students
processStudents :: [StudentGrades] -> String
processStudents students =
  let results = map processOne students
      distinctionStudents = filter (\(_, _, _, d) -> d) results
      topPerformer = if null results 
                     then Nothing
                     else Just $ maximumBy (comparing (\(_, _, a, _) -> a)) results
  in buildJSON results distinctionStudents topPerformer
  where
    processOne (sid, sname, grades) =
      let avg = calculateAverage grades
          dist = hasDistinction avg
      in (sid, sname, avg, dist)

buildJSON :: [(Int, String, Double, Bool)] -> [(Int, String, Double, Bool)] -> Maybe (Int, String, Double, Bool) -> String
buildJSON allStudents distinctionStudents topPerformer =
  "{\n  \"all_students\": [\n" ++
  intercalate ",\n" (map (\(sid, sname, avg, dist) -> 
    formatStudentResult sid sname avg dist) allStudents) ++
  "\n  ],\n" ++
  "  \"distinction_students\": [\n" ++
  intercalate ",\n" (map (\(sid, sname, avg, dist) -> 
    formatStudentResult sid sname avg dist) distinctionStudents) ++
  "\n  ],\n" ++
  "  \"top_performer\": " ++
  (case topPerformer of
    Just (sid, sname, avg, dist) -> 
      formatStudentResult sid sname avg dist ++ "\n"
    Nothing -> "null\n") ++
  "}\n"

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
  -- Read database from JSON file
  jsonContent <- readFile "main_database.json"
  
  -- Parse students from JSON
  let students = parseStudentsFromJSON jsonContent
  
  -- Process and output results
  putStrLn $ processStudents students