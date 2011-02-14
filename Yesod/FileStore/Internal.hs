{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances 
           , FlexibleContexts
           , TypeFamilies
           , FunctionalDependencies
           , UndecidableInstances
           , TypeSynonymInstances
           , ScopedTypeVariables
           #-}
module Yesod.FileStore.Internal where

import Control.Applicative
import Data.FileStore
import Yesod

import Yesod.FileStore.Types

concatPath :: FilePath -> FilePath -> FilePath
concatPath [] f = f
concatPath d f | d `endsWith` '/' = d ++ f
               | otherwise        = d ++ '/':f
  where endsWith s c  = c == last s

routeToMasterH :: Route sub -> GHandler sub y (Route y)
routeToMasterH r = do rtm <- getRouteToMaster
                      return $ rtm r

routeToMasterW :: Route sub -> GWidget sub y (Route y)
routeToMasterW = lift . routeToMasterH

class Monad m => RouteToMaster sub y m | m -> y, m -> sub where
  routeToMaster :: Route sub -> m (Route y)

instance ( sub ~ sub'
         , y ~ y'
         ) => RouteToMaster sub y (GWidget sub' y') where
  routeToMaster = routeToMasterW

instance ( sub ~ sub'
         , y ~ y'
         ) => RouteToMaster sub y (GHandler sub' y') where
  routeToMaster = routeToMasterH
  
class ( Monad m
      , RouteToMaster (FS y meta) y m
      ) => TagToMaster y meta m | m -> y, m -> meta where
  tagToMaster :: FST -> m (Route y)

instance ( Monad m
         , RouteTag y meta
         , RouteToMaster (FS y meta) y m
         ) => TagToMaster y meta m where
  tagToMaster t = routeToMaster r
    where r = routeTag (undefined::y) (undefined::meta) t :: Route (FS y meta)

class (Monad m
      ,Functor m
      ) => GetYesodSubG m r | m -> r where
  getYesodSubG :: m r

instance FSCxt y meta => GetYesodSubG (GWidget (FS y meta) y) (FS y meta) where
  getYesodSubG = lift getYesodSub

instance FSCxt y meta => GetYesodSubG (GHandler (FS y meta) y) (FS y meta) where
  getYesodSubG = getYesodSub

getFileStore :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m FileStore
getFileStore = fsFileStore <$> getYesodSubG

getLayout :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (Layout y meta)
getLayout = fsLayout <$> getYesodSubG

getFormRenderer :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (FSFormRenderer y meta)
getFormRenderer = fsFormRenderer <$> getYesodSubG

getReadView :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (ReadView y meta)
getReadView = fsReadView <$> getYesodSubG

getRevisionView :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (RevisionView y meta)
getRevisionView = fsRevisionView <$> getYesodSubG

getContentView :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (ContentView y meta)
getContentView = fsContentView <$> getYesodSubG

getAuthorView :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (AuthorView y meta)
getAuthorView = fsAuthorView <$> getYesodSubG

getFormletField :: (FSCxt y meta, GetYesodSubG m (FS y meta)) => m (FSFormletField y meta)
getFormletField = fsFormletField <$> getYesodSubG

