{-# LANGUAGE ImplicitParams, ExistentialQuantification,
    MultiParamTypeClasses, RankNTypes, ConstraintKinds #-}

{- |
    
    Module      :  Control.Logging
    Copyright   :  (C) 2016 Rev. Johnny Healey
    License     :  LGPL-3
    Maintainer  :  Rev. Johnny Healey <rev.null@gmail.com>
    Stability   :  experimental
    Portability :  unknown

    This library provides a simple framework for adding logging to an
    application. Log configuration is passed around via implicit parameters.
    Each logging configuration can carry around some bit of state that can
    be used to annotate log lines.
-}

module Control.Logging (
                        -- * Log Levels
                        Level(..)
                        -- * Log Annotations
                       ,LogFormatter
                       ,getLogLevel
                       ,getLogContext
                       ,LogAnnotation(..)
                       ,LATime(..)
                       ,LAContext(..)
                       ,LALevel(..)
                       ,LAThread(..)
                       ,LogConfig(..)
                       ,LogHeader(..)
                       -- * Log Configuration
                       ,defaultLogConfig
                       ,fileLogConfig
                       ,handleLogConfig
                       -- * Log Runners
                       ,Logging
                       ,runLogging
                       ,withLogContext
                       ,withLogHeader
                       ,withLogLevel
                        -- * Log Invocation
                       ,logLine
                       ,logPrint
                       ,debug
                       ,printDebug
                       ,info
                       ,printInfo
                       ,warn
                       ,printWarn
                       ,err
                       ,printErr
                       ,crit
                       ,printCrit
                       ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Time
import System.IO
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

-- | 'Logging' is just a Constraint synonym to denote that logging is
--   available in a function.
type Logging c = ?log :: LogConfig c

-- | Log Levels
data Level = 
    Debug
  | Info
  | Warn
  | Err
  | Crit
  deriving (Read, Show, Eq, Ord, Enum)

-- | The 'LogFormatter' monad is for converting a 'LogAnnotation' to 'String'.
type LogFormatter c = MaybeT (ReaderT (c, Level) IO)

-- | A 'LogAnnotation' is a typeclass to establish that an annotation can be
--   used for logging in the log context 'c'.
class LogAnnotation c l where
    logFormat :: l -> LogFormatter c ShowS
 
-- | Returns the current log 'Level'.
getLogLevel :: LogFormatter c Level
getLogLevel = lift $ asks snd

-- | Returns the current log context.
getLogContext :: LogFormatter c c
getLogContext = lift $ asks fst

instance LogAnnotation c String where
    logFormat = return . showString

-- | 'LogAnnotation' to log the 'Level',
data LALevel = LALevel

instance LogAnnotation c LALevel where
    logFormat _ = shows <$> getLogLevel

-- | 'LogAnnotation' to log the current time. The 'String' argument should
-- provide the desired time formatting string.
newtype LATime = LAT String

instance LogAnnotation c LATime where
    logFormat (LAT fmt) = format <$> liftIO (getCurrentTime) where
        format = showString . formatTime defaultTimeLocale fmt

-- | 'LogAnnotation' to log a String derived from the context.
newtype LAContext c = LC (c -> String)

instance LogAnnotation c (LAContext c) where
    logFormat (LC f) = lift $ asks (showString . f . fst)

-- | 'LogAnnotation' to log the current 'ThreadId'
data LAThread = LAThread

instance LogAnnotation c LAThread where
    logFormat _ = shows <$> liftIO myThreadId

-- | A 'LogHeader' wraps a 'LogAnnotation' with existential quantifcation.
data LogHeader c = forall l. LogAnnotation c l => LH l

-- | Log Configuration
data LogConfig c = LogConfig {
    -- | The minimum 'Level' to log
    logLevel :: Level
    -- | The list of 'LogHeader' wrapped annotations to display on every line.
   ,logHeader :: [LogHeader c]
    -- | The 'IO' command to log a line.
   ,logIO :: String -> IO ()
    -- | The user-defined context for logging.
   ,logContext :: c
}

-- | The default 'LogConfig' for logging to stdout. Takes the logContext as an
-- argument. This logs the time and 'Level'.
defaultLogConfig :: c -> LogConfig c
defaultLogConfig = LogConfig Info hdr putStrLn where
    hdr = [LH $ LAT "%F %X%Q", LH LALevel]

-- | The default 'LogConfig' that opens a file for logging.
fileLogConfig :: FilePath -> c -> IO (LogConfig c)
fileLogConfig p c = do
    h <- openFile p AppendMode
    return $ (defaultLogConfig c){ logIO = hPutStrLn h }

-- | The default 'LogConfig' that is provided a 'Handle' for logging.
handleLogConfig :: Handle -> c -> IO (LogConfig c)
handleLogConfig h c = do
    return $ (defaultLogConfig c){ logIO = hPutStrLn h }

-- | Evaluate a value with the provided 'LogConfig'
runLogging :: LogConfig c -> (Logging c => a) -> a
runLogging c a = let ?log = c in a

-- | Evaluate a value with the log context modified by the provided function.
withLogContext :: Logging c => (c -> c) -> (Logging c => a) -> a
withLogContext f = runLogging conf where
    LogConfig a b c d = ?log
    conf = LogConfig a b c (f d)

-- | Evaluate a value with an additional 'LogAnnotation' added to the header.
withLogHeader :: (Logging c, LogAnnotation c l) => l -> (Logging c => a) -> a
withLogHeader l = runLogging conf where
    LogConfig a b c d = ?log
    conf = LogConfig a (b ++ [LH l]) c d

-- | Evaluate a value with the specified minimum 'Level'.
withLogLevel :: Logging c => Level -> (Logging c => a) -> a
withLogLevel a = runLogging conf where
    LogConfig _ b c d = ?log
    conf = LogConfig a b c d

-- | Write a line to the log with the specified 'Level'.
logLine :: (MonadIO m, Logging c) => Level -> String -> m ()
logLine lev lin = logLine' where
    LogConfig ml lh io c = ?log
    logLine' = when (lev >= ml) $ liftIO $ do
        hdr'' <- flip runReaderT (c, lev) $
            forM lh $ \(LH h) -> runMaybeT (logFormat h)
        let hdr' = (fmap (. showChar ' ')) <$> hdr''
            Just hdr = (($ []) <$> mconcat hdr') <> Just lin
        liftIO $ io hdr

-- | Write an instance of 'Show' to the log with the specified 'Level'.
logPrint :: (MonadIO m, Show s, Logging c) => Level -> s -> m ()
logPrint lev = logLine lev . show

-- | Log a line at the Debug 'Level'.
debug :: (MonadIO m, Logging c) => String -> m ()
debug = logLine Debug

-- | Log an instance of 'Show' at the Debug 'Level'.
printDebug :: (MonadIO m, Show s, Logging c) => s -> m ()
printDebug = logPrint Debug

-- | Log a line at the Info 'Level'.
info :: (MonadIO m, Logging c) => String -> m ()
info = logLine Info

-- | Log an instance of 'Show' at the Info 'Level'.
printInfo :: (MonadIO m, Show s, Logging c) => s -> m ()
printInfo = logPrint Info

-- | Log a line at the Warn 'Level'.
warn :: (MonadIO m, Logging c) => String -> m ()
warn = logLine Warn

-- | Log an instance of 'Show' at the Warn 'Level'.
printWarn :: (MonadIO m, Show s, Logging c) => s -> m ()
printWarn = logPrint Warn

-- | Log a line at the Err 'Level'.
err :: (MonadIO m, Logging c) => String -> m ()
err = logLine Err

-- | Log an instance of 'Show' at the Err 'Level'.
printErr :: (MonadIO m, Show s, Logging c) => s -> m ()
printErr = logPrint Err

-- | Log a line at the Crit 'Level'.
crit :: (MonadIO m, Logging c) => String -> m ()
crit = logLine Crit

-- | Log an instance of 'Show' at the Crit 'Level'.
printCrit :: (MonadIO m, Show s, Logging c) => s -> m ()
printCrit = logPrint Crit

