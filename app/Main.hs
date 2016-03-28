{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Logging
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import qualified Data.Text as T
import           Git
import           Git.Libgit2 (MonadLg, LgRepo, lgFactory)

main = withStdoutLogging $ do
    log' "Starting"
    let repoOpts = RepositoryOptions { repoPath = "."
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    let dir = "."
    withRepository lgFactory dir $ do
        ref <- lookupReference "HEAD"
        ref' <- resolveReference "HEAD"
        case ref' of
            Just r -> do
                log' $ "Got refs: " <> T.pack (show r)
                commit   <- lookupCommit (Tagged r)
                commits <- listCommits Nothing (commitOid commit)
                log' "Here are the commits: "
                mapM_ (log' . T.pack . show) commits
            _ -> log' "No branch is checked out"
    log' "Finished"
