import System.Exit (exitFailure, exitSuccess, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process
import Data.List (intercalate)
import ConfigParser

main = do
   args <- getArgs
   config <- getConfig
   result <- case makeCommand config args of
                  Just cmd -> run config cmd
                  Nothing -> printUsage config >> return False
   if result then exitSuccess else exitFailure

makeCommand config args = requestedCommand >>= findCommand >>= commandStr
   where
      commandStr cmd = buildCommandString cmd (argpairs cmd)
      argpairs cmd = zip (listArgs' cmd) (tail args)
      listArgs' cmd = case listArgs cmd of
                           Just names -> names
                           Nothing -> []

      findCommand name = safeHead $ filter ((==name) . commandName) $ getCommands config

      requestedCommand = safeHead args
      safeHead xs = if length xs > 0 then Just (head xs) else Nothing

run config cmdStr = do 
   let verifiers = getVerifiers config
   let verifierPair = (\v -> (verifierName v, (verifierPrefix v) ++ " " ++ cmdStr))
   let prefixedCommands = map verifierPair verifiers
   resultMVars <- mapM forkRunner prefixedCommands
   maybeResults <- mapM takeMVar resultMVars
   let unwrapped = sequence maybeResults
   case unwrapped of
        Just results -> printResults results
        Nothing -> return False

forkRunner (name, cmdStr) = do
   let cmd = "bash"
   let args = ["-c", cmdStr]
   rtn <- newEmptyMVar
   forkIO $ do
      (exitcode, out, err) <- readProcessWithExitCode cmd args ""
      rtnVal <- case exitcode of
           ExitSuccess -> return $ Just (name, out)
           ExitFailure _ -> putStrLn err >> return Nothing
      putMVar rtn rtnVal
   return rtn

printResults results = if allResultsSame then success else fail
   where
      allResultsSame = all ((firstResult==) . snd) results
      success = putStrLn "OK" >> putStrLn (snd (head results)) >> return True
      fail = putStrLn "FAIL" >> putStrLn allResultsStr >> return False

      firstResult = snd (head results)
      allResultsStr = concat (map (\(n, r) -> n ++ ": " ++ r) results)

printUsage config = putStrLn usageStr
   where
      usageStr = "Usage: samesame <" ++ summary ++ "> ...args...\ni.e.\n  samesame " ++ commandUsage
      commandUsage = intercalate "\n  samesame " $ map args commands
      args cmd = (commandName cmd) ++ " " ++ (intercalate " " $ listArgs' cmd)
      listArgs' cmd = case listArgs cmd of 
                           Just xs -> xs
                           Nothing -> []
      summary = intercalate "|" $ map commandName commands
      commands = getCommands config
