{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , FlexibleContexts
           #-}

module Yesod.FileStore.Handlers.List where

import Data.FileStore
import Yesod

import Yesod.FileStore.Types
import Yesod.FileStore.Internal

resLink :: ( FSCxt y meta
           , RouteTag FST (FS y meta)
           ) => FilePath -> Resource -> Widget y meta ()
resLink dir (FSFile path) = do
  r <- tagToMaster . ReadT $ concatPath dir path
  addHamlet [$hamlet|
    <a href=@{r}> #{path}
  |]

resLink dir (FSDirectory path) = do
  r <- tagToMaster . ListT $ concatPath dir path
  addHamlet [$hamlet|
    <a href=@{r}> #{path}
  |]

getListRootR :: (FSCxt y meta, RouteTag FST (FS y meta)) => Handler y meta RepHtml
getListRootR = getListR ""

getListR :: (FSCxt y meta, RouteTag FST (FS y meta)) => String -> Handler y meta RepHtml
getListR dir = do
  store <- getFileStore
  rs <- liftIO $ directory store dir
  createR <- tagToMaster CreateT
  defaultLayout $ [$hamlet|
    <ul
      $forall r <- rs
        <li> ^{resLink' r}
    <a href="@{createR}"> New file
  |]  
  where resLink' = resLink dir

