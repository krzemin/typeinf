{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)

import TypeInfererTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    hspec $ do
        yesodSpec foundation $ do
            typeInfSpecs
