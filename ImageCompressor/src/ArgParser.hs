module ArgParser (
    ArgType(..),
    getFlagValue,
    getFlagIntValue,
    flagExist,
    getOptionnalIntFlag,
    checkArgsIntegrity,
    Arg(..)
 ) where

data ArgType = FLAGALONE | FLAGPARAM | ARGALONE deriving Eq

data Arg = Arg {
    argType :: ArgType,
    flag :: String,
    param :: String
}

readInt :: String -> Maybe Int
readInt [] = Nothing
readInt s = case reads s :: [(Int, String)] of
          [(result, "")] -> Just result
          _ -> Nothing

getFlagValue :: [String] -> String -> Maybe String
getFlagValue [] _ = Nothing
getFlagValue [arg1] _ = Nothing
getFlagValue (arg1:arg2:args) flag | arg1 == "--"++flag = Just arg2
                                   | otherwise = getFlagValue (arg2:args) flag


getFlagIntValue :: [String] -> String -> Maybe Int
getFlagIntValue [] _ = Nothing
getFlagIntValue [arg1] _ = Nothing
getFlagIntValue (arg1:arg2:args) flag | arg1 == "--"++flag = readInt arg2
                                | otherwise = getFlagIntValue (arg2:args) flag

flagExist :: [String] -> String -> Bool
flagExist [] _ = False
flagExist (arg1:args) flag | arg1 == "--"++flag = True
                                   | otherwise = flagExist args flag

argExist :: [String] -> String -> Bool
argExist [] _ = False
argExist (arg1:args) flag | arg1 == flag = True
                                   | otherwise = argExist args flag

getOptionnalIntFlag :: [String] -> String -> Maybe Int -> Maybe Int
getOptionnalIntFlag [] _ def = def
getOptionnalIntFlag [arg] flag def | arg == flag = Nothing
                                   | otherwise = def
getOptionnalIntFlag args flag def = case (flagExist args flag,
                    getFlagIntValue args flag) of
                        (True , s) -> s
                        (False, _) -> def

deleteArg :: [String] -> Arg -> [String]
deleteArg [a1] a@(Arg FLAGPARAM f _) = [a1]
deleteArg [] _ = []
deleteArg (a1:a2:r) a@(Arg FLAGPARAM f _) | a1 == "--"++f = r
                                          | otherwise = a1:a2:deleteArg r a
deleteArg (arg:rest) a@(Arg FLAGALONE f _) | arg == "--"++f = rest
                                          | otherwise = arg:deleteArg rest a
deleteArg (arg:rest) a@(Arg ARGALONE _ v) | arg == v = rest
                                          | otherwise = arg:deleteArg rest a

checkArgsIntegrity :: [String] -> [Arg] -> Bool
checkArgsIntegrity [] [] = True
checkArgsIntegrity notEmpty [] = False
checkArgsIntegrity args (a:ax) = checkArgsIntegrity (deleteArg args a) ax
