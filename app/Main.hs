{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Logging
import           Control.Applicative
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import           Data.Tagged
import qualified Data.Text as T
import           Options.Applicative
import           Text.Printf
import           Git
import           Git.Libgit2 (MonadLg, lgFactory)
import           Cli

runApp :: CliOptions -> IO ()
runApp (CliOptions dir isQuiet) = withStdoutLogging $ do
   withRepository lgFactory dir $ do
       refTarget <- lookupReference "HEAD"
       let maybeBranch = case refTarget of
            Just (RefSymbolic name) -> Just name
            _ -> Nothing
       maybeOid <- resolveReference "HEAD"
       case maybeOid of
           Just r -> do
               let branch = fromMaybe "(unknown)" maybeBranch
               log' $ T.pack $ printf "Latest commit on %s is %s " branch (show r)
               commit   <- lookupCommit (Tagged r)
               log' $ T.pack $ show $ commitLog commit
               commits <- listCommits Nothing (commitOid commit)
               log' "Here are the commits: "
               msgs <- mapM getMessage commits
               mapM_ log' msgs
           _ -> log' "No branch is checked out"
   log' "Finished"

main = execParser opts >>= runApp
    where
        opts = info (helper <*> cliOptions)
          (fullDesc <> progDesc "Analysis for London-style© commits in a Git repo")


getMessage :: MonadGit r m => CommitOid r -> m T.Text
getMessage commitOid = do
    commit <- lookupCommit commitOid
    let text = commitLog commit
    let author = commitAuthor commit
    let branch = case commitRefTarget commit of
                     RefSymbolic name -> show name
                     RefObj oid -> show oid

    let email = signatureEmail author
    let time = show $ signatureWhen author
    let commit = take 7 $ show $ untag commitOid
    let str = printf "Commit %-7s @ %v: %v (%v)" commit time text email
    return $ T.pack str
