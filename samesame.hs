import System.Exit (exitFailure, exitSuccess, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process

launchers = [("Tor 1", "TORSOCKS_CONF_FILE=torsocks-1.conf torsocks"), ("Tor 2", "TORSOCKS_CONF_FILE=torsocks-2.conf torsocks"), ("Local", "")]

main = do
   args <- getArgs
   result <- case args of
              ("ssh":serverName:[]) -> run $ buildSSHCommand serverName
              ("ssl":serverName:[]) -> run $ buildSSLCommand serverName
              ("url":url:[]) -> run $ buildCurlHashCommand url
              _ -> printUsage
   if result then exitSuccess else exitFailure

run command = do
   let prefixedCommands = map (\(n, p) -> (n, p ++ " " ++ command)) launchers
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

buildSSHCommand server =
   concat ["ssh-keygen -lf /dev/stdin 2> /dev/null <<< \"$(ssh-keyscan ", server , ")\""]

buildSSLCommand server =
   concat ["openssl s_client -servername ", server, " -connect ", server, ":443 </dev/null 2>/dev/null | openssl x509 -fingerprint -noout | grep -o -E '([0-9A-F]{2}:){19}[0-9A-F]{2}'"]

buildCurlHashCommand url =
   concat ["curl -s ", url, " | sha256sum"]

printUsage = putStrLn "Usage: hashcheck <https|ssh|url> <server.com|url>" >> return False
