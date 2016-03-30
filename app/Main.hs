{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import           Control.Logging
import           Control.Applicative
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import           Data.Tagged
import qualified Data.Text as T
import           Text.Printf
import           Git
import           Git.Libgit2 (MonadLg, lgFactory)

main = withStdoutLogging $ do
    let dir = "."
    withRepository lgFactory dir $ do
        ref' <- resolveReference "HEAD"
        case ref' of
            Just r -> do
                log' $ T.pack $ "Got refs: " ++ show r
                commit   <- lookupCommit (Tagged r)
                log' $ T.pack $ show $ commitLog commit
                commits <- listCommits Nothing (commitOid commit)
                log' "Here are the commits: "
                msgs <- mapM getMessage commits
                mapM_ log' msgs
            _ -> log' "No branch is checked out"
    log' "Finished"


getMessage :: MonadGit r m => CommitOid r -> m T.Text
getMessage commitOid = do
    commit <- lookupCommit commitOid
    let text = commitLog commit
    let author = commitAuthor commit
    let email = signatureEmail author
    let time = show $ signatureWhen author
    let str = printf "Commit %v @ %v: %v (%v)" (show $ untag commitOid) time text email
    return $ T.pack str
