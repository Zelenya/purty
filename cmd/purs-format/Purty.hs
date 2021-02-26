{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Purty
  ( format,
    run,
  )
where

import qualified "this" Args
import qualified "purs-tool-cst" CST
import qualified "componentm" Control.Monad.Component
import qualified "purs-tool-error" Error
import qualified "purs-tool-format" Format
import qualified "purs-tool-log" Log
import "rio" RIO hiding (log)
import qualified "rio" RIO.NonEmpty

format :: Log.Handle -> LByteString -> IO (Either Error.Error Utf8Builder)
format log contents = do
  Log.debug log ("Parsing contents: " <> displayShow contents)
  case CST.parse contents of
    Left err -> pure (Left (Error.new err))
    Right parsed -> do
      Log.debug log ("Parsed contents: " <> displayShow parsed)
      formatted <- Format.format log indentation parsed
      pure (Right formatted)

indentation :: Utf8Builder
indentation = "  "

run :: Args.Args -> IO ExitCode
run args =
  runComponent (Args.debug args) "purty" (Log.handle config) $ \log -> do
    result <- run' log args
    case RIO.NonEmpty.nonEmpty result of
      Just errs -> do
        for_ errs $ \err -> do
          Log.debug log (Error.format err)
          Log.info log (Error.message err)
        pure (ExitFailure 1)
      Nothing -> pure ExitSuccess
  where
    config :: Log.Config
    config =
      Log.Config
        { Log.name = "Log - main program",
          Log.verbose = Args.debug args
        }
    runComponent ::
      Bool ->
      Text ->
      Control.Monad.Component.ComponentM a ->
      (a -> IO ExitCode) ->
      IO ExitCode
    runComponent debug
      | debug =
        Control.Monad.Component.runComponentM1 (runSimpleApp . logInfo . display)
      | otherwise =
        Control.Monad.Component.runComponentM

run' :: Log.Handle -> Args.Args -> IO [Error.Error]
run' log args = case args of
  Args.Args mode _ -> case mode of
    Args.Format format' -> do
      Log.debug log "Formatting input"
      Args.withInput log format' (format log)
    Args.Validate validate' -> do
      Log.debug log "Validating input"
      Args.withValidate log validate' (format log)
    Args.Version version' -> do
      Log.debug log "Displaying version information"
      Args.writeVersion log version'
      pure []
