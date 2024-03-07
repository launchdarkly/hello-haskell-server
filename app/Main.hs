{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module Main where

import Control.Concurrent  (threadDelay)
import Control.Monad       (forever)
import Data.Text           (Text, pack)
import Data.Function       ((&))

import qualified LaunchDarkly.Server as LD
import System.Timeout (timeout)
import Text.Printf (printf, hPrintf)
import System.Environment (lookupEnv)

showMessage :: String -> IO ()
showMessage = printf "*** %s\n\n"

waitForClient :: LD.Client -> IO Bool
waitForClient client = do
    status <- LD.getStatus client
    case status of
        LD.Uninitialized -> threadDelay (1 * 1_000) >> waitForClient client
        LD.Initialized -> return True
        _anyOtherStatus -> return False

evaluate :: Maybe String -> Maybe String -> IO ()
evaluate (Just sdkKey) (Just featureFlagKey) = do
    {- Set up the context properties. This context should appear on your LaunchDarkly users dashboard soon after you run the demo. -}
    let context = LD.makeContext "example-user-key" "user" & LD.withName "Sandy"
    client <- LD.makeClient $ LD.makeConfig (pack sdkKey)
    initialized <- timeout (5_000 * 1_000) (waitForClient client)

    case initialized of
        Just True ->  do
            showMessage "SDK successfully initialized!"
            launched <- LD.boolVariation client (pack featureFlagKey) context False
            showMessage $ printf "Feature flag '%s' is %s for this context." featureFlagKey (show launched)
            {- Here we ensure that the SDK shuts down cleanly and has a chance to deliver analytics events to LaunchDarkly before the program exits. If analytics events are not delivered, the user properties and flag usage statistics will not appear on your dashboard. In a normal long-running application, the SDK would continue running and events would be delivered automatically in the background. -}
            LD.close client
        _notInitialized -> putStrLn "SDK failed to initialize"
evaluate  _ _ = putStrLn "You must define LAUNCHDARKLY_SERVER_KEY and LAUNCHDARKLY_FLAG_KEY before running this script"

main :: IO ()
main = do
    sdkKeyEnv <- lookupEnv "LAUNCHDARKLY_SERVER_KEY"
    featureFlagKeyEnv <- lookupEnv "LAUNCHDARKLY_FLAG_KEY"
    evaluate sdkKeyEnv featureFlagKeyEnv
