{-# LANGUAGE OverloadedStrings #-}
module TypeInfererTest
    ( typeInfSpecs
    ) where

import TestImport

typeInfSpecs :: Specs
typeInfSpecs =
    ydescribe "These are some example tests" $ do

        yit "loads the index and checks it looks right" $ do
            get TypeInfererR
            statusIs 200
            htmlAllContain "h1" "Enter lambda term"

