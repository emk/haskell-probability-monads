{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

{- |
Module       : Control.Monad.Maybe
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, undecidable instances)

The 'MaybeT' monad.  See
<http://www.haskell.org/haskellwiki/New_monads/MaybeT> for more widely-used
version.  Our 'Functor' instance and our implementation of '>>=' are
borrowed from there.

[Computation type:] Computations which may fail or return nothing.

[Binding strategy:] Failure returns the value 'Nothing', bypassing any
bound functions which follow.  Success returns a value wrapped in 'Just'.

[Useful for:] Building computations from steps which may fail.  No error
information is returned.  (If error information is required, see
'Control.Monad.Error'.)

-}

module Control.Monad.Maybe (
  MaybeT(..)
  -- * Limitations
  -- $Limitations

  -- * Example
  -- $MaybeExample
  ) where

import Control.Monad()
import Control.Monad.Trans()
import Control.Monad.Cont
import Control.Monad.Fix()
import Control.Monad.Reader
import Control.Monad.Writer

-- | A monad transformer which adds Maybe semantics to an existing monad.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Monad m) => Monad (MaybeT m) where
  fail _ = MaybeT (return Nothing)
  return = lift . return
  x >>= f = MaybeT (runMaybeT x >>= maybe (return Nothing) (runMaybeT . f))

instance MonadTrans MaybeT where
  lift x = MaybeT (liftM Just x)

instance (MonadCont m) => MonadCont (MaybeT m) where
  -- Again, I hope this is correct.
  callCC f = MaybeT (callCC (\c -> runMaybeT (f (wrap c))))
    where wrap :: (Maybe a -> m (Maybe b)) -> a -> MaybeT m b
          wrap c = MaybeT . c . Just

-- MonadError: MonadError has fairly weird semantics when lifted by MaybeT,
-- so let's skip it for now.

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadFix m) => MonadFix (MaybeT m) where
  -- I hope this is correct.  At a minimum, it typechecks.
  mfix f = MaybeT (mfix (maybe (return Nothing) (runMaybeT . f)))

-- MonadList: Not implemented.

-- MonadPlus: Ambiguous.  See note in introduction.

-- Requires -fallow-undecidable-instances.
instance (MonadReader r m) => MonadReader r (MaybeT m) where
  ask = lift ask
  local f m = MaybeT (local f (runMaybeT m))

-- MonadRWS: Not implemented.

-- MonadState: Not implemented.

-- Requires -fallow-undecidable-instances.
instance (MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  listen m = MaybeT (listen (runMaybeT m) >>= (return . liftMaybe))
    where liftMaybe (Nothing, _) = Nothing
          liftMaybe (Just x,  w) = Just (x,w)
  -- I'm not sure this is useful, but it's the best I can do:
  pass m = MaybeT (runMaybeT m >>= maybe (return Nothing)
                                         (liftM Just . pass . return))

{- $Limitations

The instance @MonadPlus@ is not provided, because it has ambiguous
semantics.  It could refer to either

>instance MonadPlus m => MonadPlus (MaybeT m)

...lifting the semantics of an underlying 'MaybeT' monad, or

>instance MonadPlus (MaybeT m)

...with semantics similar to @MonadPlus Maybe@.

-}

{- $MaybeExample

Here is an example that shows how to use 'MaybeT' to propagate an
end-of-file condition in the IO monad.  In the example below, both
@maybeReadLine@ and @failIfQuit@ may cause a failure, which will propagate
out to @main@ without further intervention.

>import System.Console.Readline
>import Data.Maybe
>import Control.Monad
>import Control.Monad.Trans
>import Control.Monad.Maybe
>
>-- 'MaybeIO' is the type of computations which do IO, and which may fail.
>type MaybeIO = MaybeT IO
>
>-- 'readline' already has type 'String -> IO (Maybe String)'; we just need
>-- to wrap it.
>maybeReadLine :: String -> MaybeIO String
>maybeReadLine prompt = MaybeT (readline prompt)
>
>-- Fail if 'str' equals "quit".
>failIfQuit :: (Monad m) => String -> m ()
>failIfQuit str = when (str == "quit") (fail "Quitting")
>
>-- This task may fail in several places.  Try typing Control-D or "quit" at
>-- any prompt.
>concatTwoInputs :: MaybeIO ()
>concatTwoInputs = do
>  s1 <- maybeReadLine "String 1> "
>  failIfQuit s1
>  s2 <- maybeReadLine "String 2> "
>  failIfQuit s2
>  liftIO (putStrLn ("Concatenated: " ++ s1 ++ s2))
>
>-- Loop until failure.
>main :: IO ()
>main = do
>  result <- runMaybeT concatTwoInputs
>  if isNothing result
>    then putStrLn "Bye!"
>    else main

-}
