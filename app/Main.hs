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

runApp :: CliOptions -> IO ()
runApp (CliOptions dir isQuiet) = withStderrLogging $ do
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
               commits <- listCommits Nothing (commitOid commit)
               log' "Here are the commits: "
               msgs <- mapM getMessage commits
               mapM_ (liftIO . TIO.putStrLn) $ catMaybes msgs
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

    let parents = commitParents commit
    branchCommits <- mergeBranchCommits parents
    let isMerge = not $ null branchCommits
    commitList <- mapM (fmap T.pack . shortened) branchCommits
    let pCommits = if isMerge then printf "(%d commit(s): %s) " (length branchCommits) (T.intercalate ", " commitList)
                              else ""

    shortHash <- shortened commitOid
    let category = if isMerge then " Merge" else "Commit" :: T.Text
    let extra
          | isMerge = pCommits
          | committer /= author = "(SQUASHED by " ++ T.unpack committer ++ ") "
          | otherwise = ""

    let str = printf "%s %-7s\t%v\t%s\t%s\t(%v)" category shortHash time extra text author
    return $ if isExcluded author then Nothing
                                  else Just $ T.pack str

-- A nice, Github-style commit extractor
shortened :: MonadGit r m => CommitOid r -> m String
shortened = return . take 7 . show . untag

-- For each merge commit, pull the history of that branch and return as a list of IDs
mergeBranchCommits :: MonadGit r m => [CommitOid r] -> m [CommitOid r]
mergeBranchCommits (mum : dad : _) = listCommits (Just mum) dad
mergeBranchCommits _ = return []

printTime:: ZonedTime -> String
printTime = formatTime defaultTimeLocale "%Y-%m-%d"

-- Not everyone is worth looking at, so exclude known bots etc
isExcluded :: CommitEmail -> Bool
isExcluded email = email `elem` ["servbot9000@crowdmix.me"]

-- TODO: Validate all non-merge commits are found earlier on a non-`master` branch
