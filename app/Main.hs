{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Logging
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Monoid
import           Data.Tagged
import qualified Data.Text as T
import           Data.Time (formatTime, knownTimeZones, defaultTimeLocale, ZonedTime)
import           Options.Applicative
import           Text.Printf
import           Git
import           Git.Libgit2 (MonadLg, lgFactory)
import           Cli

infixr +++
(+++) l r = l `T.append` r

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
               mapM_ log' $ catMaybes msgs
           _ -> log' "No branch is checked out"
   log' "Finished"

main = execParser opts >>= runApp
    where
        opts = info (helper <*> cliOptions)
          (fullDesc <> progDesc "Analysis for London-style© commits in a Git repo")


-- Format a human text message from a given CommitOid
getMessage :: MonadGit r m => CommitOid r -> m (Maybe T.Text)
getMessage commitOid = do
    commit <- lookupCommit commitOid
    let text = head $ T.lines $ commitLog commit
    let authorSig = commitAuthor commit
    let time = printTime $ signatureWhen authorSig
    let author = signatureEmail authorSig
    let committer = signatureEmail $ commitCommitter commit
    let isMerge = length (commitParents commit) > 1
    let branch = case commitRefTarget commit of
                     RefSymbolic name -> show name
                     RefObj oid -> show oid

    let commit = take 7 $ show $ untag commitOid
    let category = if isMerge then "Merge"::T.Text else "Commit"
    let warning = if committer /= author && not isMerge then "(SQUASHED by "  +++ committer +++ ") "
                                                        else ""
    let str = printf "%s %-7s @ %v: %s%v (%v)" category commit time warning text author
    return $ if isMerge || isExcluded author then Nothing
                                             else Just $ T.pack str

printTime:: ZonedTime -> String
printTime = formatTime defaultTimeLocale "%Y-%m-%d"

isExcluded :: CommitEmail -> Bool
isExcluded email = email `elem` ["servbot9000@crowdmix.me"]

-- TODO: Validate all non-merge commits are found earlier on a non-`master` branch
