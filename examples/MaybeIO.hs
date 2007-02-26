import System.Console.Readline
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe

-- 'MaybeIO' is the type of computations which do IO, and which may fail.
type MaybeIO = MaybeT IO

-- 'readline' already has type 'String -> IO (Maybe String)'; we just need
-- to wrap it.
maybeReadLine :: String -> MaybeIO String
maybeReadLine prompt = MaybeT (readline prompt)

-- Fail if 'str' equals "quit".
failIfQuit :: (Monad m) => String -> m ()
failIfQuit str = when (str == "quit") (fail "Quitting")

-- This task may fail in several places.  Try typing Control-D or "quit" at
-- any prompt.
concatTwoInputs :: MaybeIO ()
concatTwoInputs = do
  s1 <- maybeReadLine "String 1> "
  failIfQuit s1
  s2 <- maybeReadLine "String 2> "
  failIfQuit s2
  liftIO (putStrLn ("Concatenated: " ++ s1 ++ s2))

-- Loop until failure.
main :: IO ()
main = do
  result <- runMaybeT concatTwoInputs
  if isNothing result
    then putStrLn "Bye!"
    else main
