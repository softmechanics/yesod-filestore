{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , RankNTypes
           #-}
module Yesod.FileStore.Types where

import Data.FileStore
import Yesod
import Yesod.Form.Core

-- The contents of a file
type FSContent = String

class ( Yesod y
      , Read meta
      , Show meta
      , RouteTag y meta
      ) => FSCxt y meta
instance ( Yesod y
         , Read meta
         , Show meta
         , RouteTag y meta
         ) => FSCxt y meta

data FSCxt y meta => FS y meta = FS
  {
  ---- REQUIRED ----
    fsFileStore :: FileStore
  , fsFormletField :: FSFormletField y meta

  ---- OPTIONAL ----
  , fsLayout :: Layout y meta
  , fsFormRenderer :: FSFormRenderer y meta

  -- get current author for revisions
  , fsAuthor :: Handler y meta Author

  -- Generate new filepath
  , fsGenFilePath :: Handler y meta FilePath

  -- Views
  , fsAuthorView :: AuthorView y meta
  , fsRevisionView :: RevisionView y meta
  , fsContentView :: ContentView y meta
  , fsReadView :: ReadView y meta
  }

type Handler y m = GHandler (FS y m) y
type Widget y m = GWidget (FS y m) y
type FSFormletField y meta = FormletField (FS y meta) y meta 
type FSFormRenderer y meta = [[FieldInfo (FS y meta) y]] -> Widget y meta ()

type Layout y m = Widget y m () -> Handler y m RepHtml

type AuthorView y m = Author -> Widget y m ()
type RevisionView y m = Revision -> Widget y m ()
type ReadView y m = FilePath -> Revision -> m -> Widget y m ()
type ContentView y m = m -> Widget y m ()

data FST = CreateT
         | ListRootT
         | ListT String
         | ReadT String

class RouteTag y meta where
  routeTag :: y -> meta -> FST -> Route (FS y meta)

