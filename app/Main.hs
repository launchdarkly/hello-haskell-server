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
import Data.Time (getCurrentTime)
import Data.Time.Format
import Data.Maybe (isJust)

showEvaluationResult :: String -> Bool -> IO ()
showEvaluationResult key value = do
    now <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%H:%M:%S" now
    printf "*** %s: The %s feature flag evaluates to %s\n" formattedTime key (show value)

showBanner :: IO ()
showBanner = putStr "\n\
\        ██       \n\
\          ██     \n\
\      ████████   \n\
\         ███████ \n\
\██ LAUNCHDARKLY █\n\
\         ███████ \n\
\      ████████   \n\
\          ██     \n\
\        ██       \n\
\\n\
\"

showMessage :: String -> Bool -> Maybe Bool -> Bool -> IO Bool
showMessage key True _ True = do
    showBanner
    showEvaluationResult key True
    pure False
showMessage key value Nothing showBanner = do
    showEvaluationResult key value
    pure showBanner
showMessage key value (Just lastValue) showBanner
    | value /= lastValue = do
        showEvaluationResult key value
        pure showBanner
    | otherwise = pure showBanner

waitForClient :: LD.Client -> IO Bool
waitForClient client = do
    status <- LD.getStatus client
    case status of
        LD.Uninitialized -> threadDelay (1 * 1_000) >> waitForClient client
        LD.Initialized -> return True
        _anyOtherStatus -> return False

evaluateLoop :: LD.Client -> String -> LD.Context -> Maybe Bool -> Bool -> Bool -> IO ()
evaluateLoop client featureFlagKey context lastValue showBanner ciMode = do
    value <- LD.boolVariation client (pack featureFlagKey) context False
    showBanner' <- showMessage featureFlagKey value lastValue showBanner

    if ciMode then pure () else threadDelay (1 * 1_000_000) >> evaluateLoop client featureFlagKey context (Just value) showBanner' False

evaluate :: Maybe String -> Maybe String -> Bool -> IO ()
evaluate (Just sdkKey) Nothing ciMode = do evaluate (Just sdkKey) (Just "sample-feature") ciMode
evaluate (Just sdkKey) (Just featureFlagKey) ciMode = do
    -- Set up the evaluation context. This context should appear on your
    -- LaunchDarkly contexts dashboard soon after you run the demo.
    let context = LD.makeContext "example-user-key" "user" & LD.withName "Sandy"
    client <- LD.makeClient $ LD.makeConfig (pack sdkKey)
    initialized <- timeout (5_000 * 1_000) (waitForClient client)

    case initialized of
        Just True ->  do
            print "*** SDK successfully initialized!"
            evaluateLoop client featureFlagKey context Nothing True ciMode
        _notInitialized -> putStrLn "*** SDK failed to initialize. Please check your internet connection and SDK credential for any typo."
evaluate  _ _ _ = putStrLn "*** You must define LAUNCHDARKLY_SERVER_KEY and LAUNCHDARKLY_FLAG_KEY before running this script"

main :: IO ()
main = do
    -- Set sdkKey to your LaunchDarkly SDK key.
    sdkKey <- lookupEnv "LAUNCHDARKLY_SERVER_KEY"
    -- Set featureFlagKey to the feature flag key you want to evaluate.
    featureFlagKey <- lookupEnv "LAUNCHDARKLY_FLAG_KEY"
    ciMode <- lookupEnv "CI"
    evaluate sdkKey featureFlagKey (isJust ciMode)
