{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent  (threadDelay)
import Control.Monad       (forever)
import Data.Text           (Text)

import LaunchDarkly.Server (makeUser, makeConfig, makeClient, boolVariation)

yourSDKKey :: Text
yourSDKKey = "<put your SDK key here>"

yourFlagKey :: Text
yourFlagKey = "<put your feature key here>"

main :: IO ()
main = do
    let user = makeUser "abc"
    client <- makeClient $ makeConfig yourSDKKey
    forever $ do
        launched <- boolVariation client yourFlagKey user False
        putStrLn $ "Flag is: " ++ show launched
        -- one second
        threadDelay $ 1 * 1000000
