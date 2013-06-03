module Handler.TypeInferer where

import Import
import Data.Text
import TypeInf.TypeInf

getTypeInfererR :: Handler RepHtml
getTypeInfererR = defaultLayout $(widgetFile "typeinf")

postTypeInfererR :: Handler RepHtml
postTypeInfererR = do
  lambdaTermText <- runInputPost $ ireq textField "lambdaTerm"
  let lambdaTermStr = Data.Text.unpack lambdaTermText
  let (contextStr, typeStr) = TypeInf.TypeInf.parseAndShowContextAndType lambdaTermStr
  defaultLayout $(widgetFile "typeinf_ans")
