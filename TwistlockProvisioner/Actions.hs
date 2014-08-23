module TwistlockProvisioner.Actions where
import System.Process
import System.Exit
import System.IO

echoHelloWorld :: IO ExitCode
echoHelloWorld = do
  (inp, outp, err, handle) <- runInteractiveCommand "./hello-world"
  hPutStrLn inp "Haskell World"
  hFlush inp
  message <- hGetLine outp
  putStrLn message
  waitForProcess handle
