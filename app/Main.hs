{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Logging (withStderrLogging, log')
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid
import           Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (formatTime, knownTimeZones, defaultTimeLocale, ZonedTime)
import           Options.Applicative
import           Text.Printf
import           Git
import           Git.Libgit2 (MonadLg, lgFactory)
import           Cli

infixr +++
(+++) l r = l `T.append` r

-- Takes options and outputs dodgy stuff
runApp :: CliOptions -> IO ()
runApp (CliOptions excludes verbose dir) = withStderrLogging $ do
    when verbose $ log' $ "Excluding these emails: " +++ T.intercalate ", " excludes

    withRepository lgFactory dir $ do
        refTarget <- lookupReference "HEAD"
        let maybeBranch = case refTarget of
                              Just (RefSymbolic name) -> Just name
                              _ -> Nothing
        maybeOid <- resolveReference "HEAD"
        case maybeOid of
            Just r -> do
                let branch = fromMaybe "(unknown)" maybeBranch
                when verbose $ log' $ T.pack $ printf "Latest commit on %s is %s " branch (show r)
                commit   <- lookupCommit (Tagged r)
                commits <- listCommits Nothing (commitOid commit)
                tuples  <- mapM (processCommits excludes) commits
                let branchComs = concatMap snd tuples
                let realCommits = mapMaybe fst tuples
                branchHashes <- listHashes branchComs
                when verbose $ log' ("Got these branch commits: " +++ branchHashes)
                let dodgyCommits = filter (isKnownBranch branchComs) realCommits
                when verbose $ log' $ T.pack $ printf "%d dodgy commits found out of %d non-merge commits"
                                              (length dodgyCommits) (length realCommits)
                pretty <- mapM formatCommit dodgyCommits
                mapM_ (liftIO . TIO.putStrLn) pretty
            _ -> log' "No branch is checked out"
    when verbose $ log' "Finished"

isKnownBranch :: Eq a => [a] -> a -> Bool
isKnownBranch = flip notElem

-- Convenience method for displaying a list of short-form commits
listHashes :: MonadGit r m => [CommitOid r] -> m T.Text
listHashes commits = do
    hashes <- mapM (fmap T.pack . shortened) commits
    return $ T.intercalate ", " hashes


-- Split a commit into a candidate (or Nothing) or a list of any merged commits it included
-- TODO: migrate to Either CommitOid [CommitOid]?
processCommits :: MonadGit r m => [T.Text]                                  -- Excluded emails
                              -> CommitOid r                                -- One to look into
                              -> m (Maybe (CommitOid r), [CommitOid r])     -- A commit to keep, list of branch commits
processCommits excludes commitOid = do
    commit <- lookupCommit commitOid
    let authorSig = commitAuthor commit
    let author = signatureEmail authorSig
    let committer = signatureEmail $ commitCommitter commit

    let parents = commitParents commit
    branchCommits <- mergeBranchCommits parents
    let isMerge = not $ null branchCommits
    let result
          | isMerge                                           = (Nothing, branchCommits)
          | isExcluded excludes author || committer /= author = (Nothing, [])
          | otherwise                                         = (Just commitOid, [])
    return result


-- Look up info for the given CommitOid and format nicely
formatCommit :: MonadGit r m => CommitOid r -> m T.Text
formatCommit commitOid = do
    commit <- lookupCommit commitOid
    let headline = head $ T.lines $ commitLog commit
    let authorSig = commitAuthor commit
    let time = printTime $ signatureWhen authorSig
    let author = signatureEmail authorSig
    let committer = signatureEmail $ commitCommitter commit
    let isMerge = length (commitParents commit) > 1
    shortHash <- shortened commitOid
    let extra
          | committer /= author = ", committed by " ++ T.unpack committer
          | otherwise = ""
    let str = printf "%-7s\t%s\t%s\t(%v%s)" shortHash time headline author extra
    return $ T.pack str


-- A nice, Github-style commit extractor
shortened :: MonadGit r m => CommitOid r -> m String
shortened = return . take 7 . show . untag


-- For each merge commit, pull the history of that branch and return as a list of IDs
mergeBranchCommits :: MonadGit r m => [CommitOid r] -> m [CommitOid r]
mergeBranchCommits (mum : dad : _) = listCommits (Just mum) dad
mergeBranchCommits _ = return []

-- Default formatting is too long / ugly...
printTime:: ZonedTime -> String
printTime = formatTime defaultTimeLocale "%Y-%m-%d"

-- Not everyone is worth looking at, so exclude known bots etc
isExcluded :: [T.Text] -> CommitEmail -> Bool
isExcluded excludes email = email `elem` excludes

main :: IO ()
main = execParser opts >>= runApp
    where
        opts = info (helper <*> cliOptions)
          (fullDesc <> progDesc "Analysis for London-style© commits in a Git repo")
