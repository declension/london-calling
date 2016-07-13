{-# LANGUAGE OverloadedStrings #-}
module Cli where

import           Options.Applicative
import qualified Data.Text as T

data CliOptions = CliOptions { excludedEmails :: [T.Text]
                             , verbose :: Bool
                             , dir :: String}

parseCommaSeparated :: Monad m => String -> m [T.Text]
parseCommaSeparated all@(x : xs) = return $ T.splitOn "," (T.pack all)
parseCommaSeparated _ = return []

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> option (str >>= parseCommaSeparated) (long "exclude" <> value [] <> metavar "EMAIL"
                    <> help "comma-separated email(s) to exclude from")
  <*> switch       (long "debug" <> short 'd'
                    <> help "Print logging to STDERR")
  <*> argument str (metavar "GIT-DIR"
                    <> help "Git directory")
