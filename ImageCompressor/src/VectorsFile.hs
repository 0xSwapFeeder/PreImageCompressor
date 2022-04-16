module VectorsFile where

import Vectors

import Lib



stringListToIntList :: [String] -> Maybe [Int]
stringListToIntList [] = Just []
stringListToIntList (x:xs) = case (stringListToIntList xs, readInt x)of 
                                (Nothing, _) -> Nothing
                                (_, Nothing) -> Nothing
                                (Just as, Just a) -> Just (a : as)

readInt :: String -> Maybe Int
readInt [] = Nothing
readInt s = case reads s :: [(Int, String)] of
          [(result, "")] -> Just result
          _ -> Nothing

isCharThere :: String -> Char -> Bool
isCharThere [] c = False
isCharThere (x:xs) c | x == c = True
                     | otherwise = isCharThere xs c

stringToStringListTerm :: String -> Char -> Char -> [String]
stringToStringListTerm [] _ _ = []
stringToStringListTerm [last] _ t = []
stringToStringListTerm s c t
                       | last s /= t = []
                       | isCharThere s c = takeWhile (/= c) s :
     stringToStringListTerm (tail (dropWhile (/= c) s)) c t
                       | otherwise = [takeWhile (/= t) s]

stringToStringList :: String -> Char -> [String]
stringToStringList [] _ = []
stringToStringList s c | isCharThere s c = takeWhile (/= c) s :
     stringToStringList (tail (dropWhile (/= c) s)) c
                       | otherwise = [s]



toVector3i :: String -> Maybe Vector3i
toVector3i [] = Nothing
toVector3i ('(':s) = case stringListToIntList $ stringToStringListTerm s ',' ')' of
                Nothing -> Nothing
                Just [x,y,z] -> Just (x, y, z)
                _ -> Nothing
toVector3i _ = Nothing

toVector2i :: String -> Maybe Vector2i
toVector2i [] = Nothing
toVector2i ('(':s) = case stringListToIntList $ stringToStringListTerm s ',' ')' of
                Nothing -> Nothing
                Just [x,y] -> Just (x, y)
                _ -> Nothing
toVector2i _ = Nothing

--toVectors3i :: String -> Maybe [Vector3i]
--toVectors3i [] = Just []
--toVectors3i ([]:_) = Nothing
--toVectors3i ('(':xs) = case (toVector3i $ stringToStringListTerm x ',' ')', toVectors3i xs) of
--                            (Nothing, Nothing)-> Nothing
--                            (_, Nothing) -> Nothing
--                            (Nothing, _ ) -> Nothing
--                            (Just v, Just rest) ->  Just $ v : rest

--toVectors3i ((_:x):xs) = Nothing


--fileToVectors3i :: String -> Maybe [Vector3i]
--fileToVectors3i args = toVectors3i $ stringToStringList args '\n'





split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

pixelFromLine :: String -> Maybe PixelUnit
pixelFromLine line = case (toVector2i $ head splitted, toVector3i $ last splitted) of
                                (Nothing, _) -> Nothing
                                (_, Nothing) -> Nothing
                                (Just p, Just v) -> Just PixelUnit{position=p, value=v}
                                where splitted = split ' ' line


extractLines :: String -> [String]
extractLines = split '\n'

fileToPixelsLoop :: [String] -> Maybe [PixelUnit]
fileToPixelsLoop [] = Just []
fileToPixelsLoop (x:xs) = case pixelFromLine x of
                                Nothing -> Nothing
                                Just p -> case fileToPixelsLoop xs of
                                                Nothing -> Nothing
                                                Just ps -> Just (p : ps)
--fileToPixelsLoop (x:xs) = case pixelFromLine x of 
--                              Just p -> p : fileToPixelsLoop xs
--                              Nothing -> Nothing


fileToPixels :: String -> Maybe [PixelUnit]
fileToPixels file = fileToPixelsLoop $ extractLines file