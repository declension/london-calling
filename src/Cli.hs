module Cli where

import           Options.Applicative

data CliOptions = CliOptions {dir :: String, quiet :: Bool}

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> argument str (metavar "GIT-DIR" <> help "Git directory")
  <*> switch       (long "quiet" <> help "Whether to be quiet")
