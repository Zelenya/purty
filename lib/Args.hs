module Args
  ( Args(..)
  , debug
  , info
  , input
  , parse
  , withInput
  ) where

import "rio" RIO hiding (log)

import qualified "bytestring" Data.ByteString.Builder
import qualified "this" Error
import qualified "this" Log
import qualified "optparse-applicative" Options.Applicative
import qualified "rio" RIO.ByteString.Lazy
import qualified "rio" RIO.Directory
import qualified "rio" RIO.File

newtype Args
  = Format Format

data Format
  = Format' Input Output Verbose

instance Display Format where
  display format' = case format' of
    Format' input'' output' verbose' ->
      "Format {"
        <> " input = "
        <> display input''
        <> ","
        <> " output = "
        <> display output'
        <> ","
        <> " verbose = "
        <> display verbose'
        <> " }"

data Input
  = InputFile FilePath
  | InputSTDIN

instance Display Input where
  display input'' = case input'' of
    InputFile file ->
      "InputFile {"
        <> " filePath = "
        <> displayShow file
        <> " }"
    InputSTDIN ->
      "InputSTDIN {"
        <> " }"

data Output
  = STDOUT
  | Write

instance Display Output where
  display output' = case output' of
    STDOUT -> "STDOUT"
    Write  -> "Write"

instance Semigroup Output where
  output1 <> output2 = case (output1, output2) of
    (STDOUT, STDOUT) -> STDOUT
    (STDOUT, Write)  -> Write
    (Write, STDOUT)  -> Write
    (Write, Write)   -> Write

data Verbose
  = NotVerbose
  | Verbose

instance Display Verbose where
  display verbose' = case verbose' of
    NotVerbose -> "NotVerbose"
    Verbose    -> "Verbose"

instance Semigroup Verbose where
  verbose1 <> verbose2 = case (verbose1, verbose2) of
    (NotVerbose, NotVerbose) -> NotVerbose
    (NotVerbose, Verbose)    -> Verbose
    (Verbose, NotVerbose)    -> Verbose
    (Verbose, Verbose)       -> Verbose

args :: Options.Applicative.Parser Args
args =
  asum
    [ fmap Format format
    ]

debug :: Args -> Bool
debug args' = case args' of
  Format format'     -> debugFormat format'

debugFormat :: Format -> Bool
debugFormat format' = case format' of
  Format' _ _ verbose' -> debugVerbose verbose'

debugVerbose :: Verbose -> Bool
debugVerbose verbose' = case verbose' of
  NotVerbose -> False
  Verbose    -> True

format :: Options.Applicative.Parser Format
format =
  pure Format'
    <*> input'
    <*> output
    <*> verbose

info :: Options.Applicative.ParserInfo Args
info = Options.Applicative.info (Options.Applicative.helper <*> args) description
  where
  description :: Options.Applicative.InfoMod Args
  description =
    Options.Applicative.fullDesc
      <> Options.Applicative.progDesc "Pretty print a PureScript file"
      <> Options.Applicative.header "purty - A PureScript pretty-printer"

input :: Format -> Utf8Builder
input format' = case format' of
  Format' InputSTDIN _ _       -> "STDIN"
  Format' (InputFile file) _ _ -> displayShow file

input' :: Options.Applicative.Parser Input
input' =
  Options.Applicative.argument input'' meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.ArgumentFields a
  meta =
    Options.Applicative.help "PureScript file to format or `-` for STDIN"
      <> Options.Applicative.metavar "FILE"

  input'' :: Options.Applicative.ReadM Input
  input'' = Options.Applicative.maybeReader $ \str -> case str of
    "-" -> Just InputSTDIN
    _   -> Just (InputFile str)

output :: Options.Applicative.Parser Output
output = Options.Applicative.flag STDOUT Write meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Format file in-place"
      <> Options.Applicative.long "write"

parse :: IO Args
parse = Options.Applicative.execParser info

verbose :: Options.Applicative.Parser Verbose
verbose = Options.Applicative.flag NotVerbose Verbose meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Print debugging information to STDERR while running"
      <> Options.Applicative.long "verbose"

withInput ::
  Log.Handle ->
  Format ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  IO [Error.Error]
withInput log format' f = case format' of
  Format' (InputFile file') output' _ -> do
    Log.debug log ("Converting file " <> displayShow file' <> " to absolute.")
    file <- RIO.Directory.makeAbsolute file'
    err' <- write log f output' file
    case err' of
      Just err -> pure [err]
      Nothing -> pure []
  Format' InputSTDIN _ _ -> do
    Log.debug log "Reading STDIN."
    result' <- tryIO RIO.ByteString.Lazy.getContents
    case result' of
      Left err ->
        pure [Error.new ("Error reading STDIN: " <> displayShow err)]
      Right contents -> do
        Log.debug log "Got STDIN contents"
        result <- f contents
        case result of
          Left err ->
            pure [Error.wrap "Error formatting STDIN" err]
          Right formatted -> do
            Log.debug log "Writing formatted STDIN to STDOUT"
            hPutBuilder stdout (getUtf8Builder formatted)
            Log.debug log "Wrote formatted STDIN to STDOUT"
            pure []

write ::
  Log.Handle ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  Output ->
  FilePath ->
  IO (Maybe Error.Error)
write log f output' file = do
  Log.debug log ("Reading " <> displayShow file <> ".")
  result' <- tryIO $ withLazyFile file $ \contents -> do
    Log.debug log "Got the file contents"
    result <- f contents
    Log.debug log "Finished with the file"
    pure result
  case result' of
    Left err ->
      pure (Just $ Error.new $ "Error reading file: " <> displayShow err)
    Right result -> case result of
      Left err ->
        pure (Just (Error.wrap ("Error formatting " <> displayShow file) err))
      Right formatted -> case output' of
        STDOUT -> do
          Log.debug log "Writing formatted file to STDOUT"
          hPutBuilder stdout (getUtf8Builder formatted)
          Log.debug log "Wrote formatted file to STDOUT"
          pure Nothing
        Write -> do
          Log.debug log ("Writing formatted file " <> displayShow file <> " in-place.")
          RIO.File.writeBinaryFileDurableAtomic
            file
            ( toStrictBytes
              $ Data.ByteString.Builder.toLazyByteString
              $ getUtf8Builder formatted
            )
          Log.debug log "Wrote formatted file in-place"
          pure Nothing
