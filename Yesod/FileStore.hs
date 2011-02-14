{-# LANGUAGE QuasiQuotes
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses 
           , UndecidableInstances
           , TemplateHaskell
           #-}
module Yesod.FileStore (
    FS (..)
  , FSRoute (..)
  , defaultFS
  , module Handlers
  ) where

import Control.Applicative
import Data.DateTime
import Data.FileStore
import Language.Haskell.TH.Syntax hiding (ListT)
import Yesod

import Yesod.FileStore.Types 
import Yesod.FileStore.Handlers as Handlers
import Yesod.FileStore.Views
import Yesod.Form.Core

-- TODO: maybe: FormTag to identify the form for: rendering, combining subform widgets
-- fsRenderForm :: FSFormComponentTag -> FSFormRenderer y meta
-- fsWidget :: FSFormTag -> [Widget y meta] -> Widget y meta

defaultFS :: FSCxt y meta => FS y meta
defaultFS = FS 
  { 
  ---- REQUIRED ----
    fsFileStore = error "Uninitialized REQUIRED FS field fsFileStore.  Did someone forget to set it?"
  , fsFormletField = error "Uninitialized REQUIRED FS field fsFormletField.  Did someone forget to set it?"

  ---- OPTIONAL ----
  , fsLayout = layout
  -- get current author for revisions
  , fsAuthor = return $ Author "Nobody" "nobody@nowhere"

  -- Generate new filepath
  , fsGenFilePath = formatDateTime "%Y-%m-%d-%I-%M-%S" <$> liftIO getCurrentTime

  -- Views
  , fsAuthorView = authorView
  , fsRevisionView = revisionView
  , fsContentView = contentView
  , fsReadView = readView

  -- 
  , fsFormRenderer = mapM_ fieldToTableRow . concat
  }

fieldToTableRow fi = [hamlet|
  <tr .#{clazz fi}>
      <td>
          <label for="#{fiIdent fi}">#{fiLabel fi}
          <div .tooltip>#{fiTooltip fi}
      <td>
          \^{fiInput fi}
      $maybe err <- fiErrors fi
          <td .errors>#{err}
  |]
  where clazz fi = if fiRequired fi then "required" else "optional"


mkYesodSub "FS master meta"
     [ ClassP (mkName "YesodSubRoute") [
          (ConT $ mkName "FS") `AppT` (VarT $ mkName "master") `AppT` (VarT $ mkName "meta")
        , VarT $ mkName "master"
        ] 
     , ClassP (mkName "FSCxt") [VarT $ mkName "master", VarT $ mkName "meta"]
     ] [$parseRoutes|
/create                 CreateR GET POST
/list                   ListRootR GET
/list/#String           ListR GET
/read/#String           ReadR GET
/                       RootR GET
|]

getRootR :: GHandler (FS sub meta) y ()
getRootR = do
  rtm <- getRouteToMaster
  redirect RedirectTemporary $ rtm ListRootR

instance RouteTag y meta where
  routeTag _ _ CreateT = CreateR
  routeTag _ _ ListRootT = ListRootR
  routeTag _ _ (ListT path) = ListR path
  routeTag _ _ (ReadT path) = ReadR path


