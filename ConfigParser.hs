{-| 
Module: ConfigParser
Description: Handles fetching and parsing of configuration information
-}
module ConfigParser (
   getConfig,
   getCommands,
   getVerifiers,
   commandName,
   verifierName,
   verifierPrefix,
   buildCommandString,
   listArgs,
   Config,
   Command
) where

import Data.List (sort, nub)

data Config = Config [Verifier] [Command] deriving Show

data Verifier = Verifier String String deriving Show

data Command = Command String String deriving Show

getConfig :: IO Config
getConfig = getConfigPath >>= parseFile

parseFile :: FilePath -> IO Config
parseFile path = readFile path >>= return . parseConfigString

parseConfigString :: String -> Config
parseConfigString str = Config verifiers commands
   where
      commands = map (\(key, val) -> Command key val) cmdPairs
      verifiers = map (\(key, val) -> Verifier key val) verifierPairs

      verifierPairs = extractFromConfig "verifier." configPairs
      cmdPairs = extractFromConfig "command." configPairs

      configPairs = foldl add [] $ filter ignorable (lines str)
      add xs l = (parse l):xs
      parse l = let (k, v') = span (/='=') l in (trim k, trim $ tail v')
      ignorable l = (l /= "") && ('#' /= head l)
      trim = f . f
         where f = reverse . dropWhile (==' ')

getCommands (Config _ commands) = commands
getVerifiers (Config verifiers _) = verifiers
commandName (Command name _) = name
verifierName (Verifier name _) = name
verifierPrefix (Verifier _ prefix) = prefix

listArgs :: Command -> Maybe [String]
listArgs (Command _ cmd) = result
   where
      result = case (foldl parse init cmd) of
         Just (_, _, args) -> Just (nub $ reverse args)
         Nothing -> Nothing
      -- First part is state machine for parse, second is buffer, third is results
      init = Just ("none", "", [])

      parse Nothing _ = Nothing
      parse (Just carry) c = parse' carry c
      parse' ("none", buffer, xs) '%' = Just ("open one", buffer, xs)
      parse' ("none", buffer, xs) _ = Just ("none", buffer, xs)
      parse' ("open one", buffer, xs) '%' = Just ("open two", buffer, xs)
      parse' ("open one", buffer, xs) _ = Just ("none", buffer, xs)
      parse' ("open two", buffer, xs) '%' = Just ("close one", buffer, xs)
      parse' ("open two", buffer, xs) c = Just ("open two", c:buffer, xs)
      parse' ("close one", buffer, xs) '%' = Just ("none", "", (reverse buffer):xs)
      parse' ("close one", buffer, xs) _ = Nothing

buildCommandString :: Command -> [(String, String)] -> Maybe String
buildCommandString cmd args = result
   where
      result = case listArgs cmd of
         Just cmdArgs -> if (hasAllArgs cmdArgs) then Just insertValues else Nothing
         Nothing -> Nothing
      hasAllArgs cmdArgs = (sort cmdArgs) == (sort (map fst args))

      insertValues = foldl addArg cmdTemplate args
      addArg str (name, value) = replace str ("%%" ++ name ++ "%%") value
      Command _ cmdTemplate = cmd

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

extractFromConfig prefix config = foldl addIfMatches [] config
   where
      addIfMatches c (key,val) = if startsWith prefix key then (strip key,val):c else c
      strip key = drop (length prefix) key
      startsWith (x:xs) (y:ys) = if x == y then startsWith xs ys else False
      startsWith [] ys = True
      startsWith xs [] = False

-- TODO: Check environment variable
getConfigPath = return "default.conf"
