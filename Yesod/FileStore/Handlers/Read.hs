{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , FlexibleContexts
           #-}

module Yesod.FileStore.Handlers.Read where

import Control.Applicative
import Data.FileStore
import Yesod

import Yesod.FileStore.Types
import Yesod.FileStore.Internal

getReadR :: FSCxt y meta => String -> Handler y meta RepHtml
getReadR fp = do
  store <- getFileStore
  revid <- liftIO $ latest store fp
  rev <- liftIO $ revision store revid
  con <- liftIO $ retrieve store fp $ Just revid

  layout <- getLayout
  view <- getReadView
  liftIO $ print con
  layout $ view fp rev $ read con


