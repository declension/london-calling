{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.String (fromString)
import Data.Maybe
import System.Environment (lookupEnv, getArgs)
import Data.List (intercalate)
import Data.Time
import Data.Vector (toList)
import GitHub.Endpoints.Repos.Commits as Github
import Data.Text as T (unpack)

main = do
  auth <- getAuth
  putStrLn "Found token, starting request..."
  possibleCommits <- Github.commitsWithOptionsFor' auth "quodlibet" "quodlibet" [CommitQueryAuthor "declension"]
  case possibleCommits of
    (Left error)    -> putStrLn $ "Error: " ++ show error
    (Right commits) -> putStrLn $ intercalate "\n\n" $ map formatCommit $ toList commits

formatCommit :: Github.Commit -> String
formatCommit commit =
  "commit " ++ show (Github.commitSha commit) ++
    "\nAuthor: " ++ formatAuthor author ++
    "\nDate:   " ++ show (Github.gitUserDate author) ++
    "\n\n\t" ++ T.unpack (Github.gitCommitMessage gitCommit)
  where author = Github.gitCommitAuthor gitCommit
        gitCommit = Github.commitGitCommit commit

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  T.unpack (Github.gitUserName author) ++ " <" ++ T.unpack (Github.gitUserEmail author) ++ ">"


getAuth :: IO (Maybe Github.Auth)
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (Github.OAuth . fromString <$> token)
