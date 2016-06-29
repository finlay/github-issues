{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import System.Environment
import Control.Monad
import Data.String

import Data.Text (Text)
import Data.Time
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.GitHub
import Options.Generic
import Data.Csv

data Options
  = Options 
    { repository  :: Text
    , fields      :: BS.ByteString
    } deriving (Generic, Show) 

instance ParseRecord Options

-- Instance for Issue from Network.GitHub.Types
instance ToField Bool where 
    toField True = "T"
    toField False = "F"
instance ToField UTCTime where
    toField = BS.pack . show
instance ToNamedRecord Issue where
    toNamedRecord issue@Issue{..} 
        = namedRecord 
            [ "number"   .= issueNumber
            , "state"    .= issueState
            , "title"    .= issueTitle
            , "body"     .= issueBody
            , "locked"   .= issueLocked
            , "comments" .= issueComments
            , "created"  .= issueCreated
            , "updated"  .= issueUpdated
            , "closed"   .= issueClosed
            ]

main :: IO()
main = do

    opts <- getRecord "Dump github issues to CSV"
    
    let repo = Text.split (== '/') (repository opts)
    when (length repo /= 2) $ do
        putStrLn "--repository argument must contain a / character"
        exitFailure

    token  <- fmap fromString <$> lookupEnv "GITHUB_TOKEN"
        
    result <- runGitHub (getIssues [("state", "all")] (repo !! 0) (repo !! 1)) token
    case result of
        Left e  ->  print e
        Right issues -> do 
            let header = Vector.fromList $ BS.split ',' $ fields opts
            LBS.putStr $ encodeByName header issues

