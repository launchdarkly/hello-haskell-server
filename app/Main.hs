{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module Main where

import Control.Concurrent  (threadDelay)
import Control.Monad       (forever)
import Data.Text           (Text)
import Data.Function       ((&))

import qualified LaunchDarkly.Server as LD
import System.Timeout (timeout)
import Text.Printf (printf, hPrintf)

 -- Set sdkKey to your LaunchDarkly SDK key before running
sdkKey :: Text
sdkKey = ""

 -- Set featureFlagKey to the feature flag key you want to evaluate
featureFlagKey :: Text
featureFlagKey = "my-boolean-flag"

showMessage :: String -> IO ()
showMessage = printf "*** %s\n\n"

waitForClient :: LD.Client -> IO Bool
waitForClient client = do
    status <- LD.getStatus client
    case status of
        LD.Uninitialized -> threadDelay (1 * 1_000) >> waitForClient client
        LD.Initialized -> return True
        _ -> return False

main :: IO ()
main
  | sdkKey == "" = showMessage "Please edit Main.hs to set sdkKey to your LaunchDarkly SDK key first"
  | otherwise = do
    let user = LD.makeUser "user@example.com" & LD.userSetName (Just "Sandy")
    client <- LD.makeClient $ LD.makeConfig sdkKey
    initialized <- timeout (5_000 * 1_000) (waitForClient client)

    case initialized of
      Just True ->  do
        showMessage "SDK successfully initialized!"
        launched <- LD.boolVariation client featureFlagKey user False
        showMessage $ printf "Feature flag '%s' is %s for this user." featureFlagKey (show launched)
        LD.close client
      _ -> putStrLn "SDK failed to initialize"
