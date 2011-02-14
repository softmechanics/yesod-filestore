{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           #-}

import Control.Applicative
import Data.FileStore.Git

import Yesod
import Yesod.Handler
import Yesod.Dispatch
import Yesod.FileStore
import Yesod.FileStore.Types

data Test = Test

data Post = Post {
    title :: String
  , body :: Textarea
  }
  deriving (Read, Show)

postFormletField params = Post
  <$> stringField "Post Title" (title <$> params)
  <*> textareaField "Post Body" (body <$> params)

postView (Post title body) = addHamlet [$hamlet|
  <tr
    <th> Title
    <td> #{title}
  <tr
    <th> Body
    <td> #{body}
  |]

fs = defaultFS { 
    fsFileStore = gitFileStore "/tmp/fs" 
  , fsFormletField = postFormletField
  , fsContentView = postView
  }

type FSTest = FS Test Post

getFS :: Test -> FSTest
getFS _ = fs

mkYesod "Test" [$parseRoutes|
/ FSR FSTest getFS
|]

instance Yesod Test where 
  approot _ = ""

instance YesodSubRoute FSTest Test where
  fromSubRoute _ _ = FSR

main = warpDebug 3000 Test

