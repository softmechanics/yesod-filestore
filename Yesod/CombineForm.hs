{-# LANGUAGE FlexibleInstances 
           , FlexibleContexts
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FunctionalDependencies
           , UndecidableInstances
           , TypeFamilies
           #-}
module Yesod.CombineForm where

import Yesod
import Yesod.Form.Core

import Data.HList hiding (tuple)
import Data.HList.Tuple
import Data.HList.Utils

-- Part 1: combine 2 forms

formFailure :: FormResult a -> [String]
formFailure (FormFailure fs) = fs
formFailure _ = []

combine2FormResults :: FormResult a -> FormResult b -> FormResult (a,b)
combine2FormResults (FormSuccess a) (FormSuccess b) = FormSuccess (a,b)
combine2FormResults FormMissing FormMissing = FormMissing
combine2FormResults a b = FormFailure $ formFailure a ++ formFailure b 

combine2Forms :: GForm s y xml f1 -> GForm s y xml f2 -> GForm s y [xml] (f1,f2)
combine2Forms (GForm f1) (GForm f2) = GForm $ do 
  (r1,x1,enc) <- f1
  (r2,x2,_) <- f2
  return (combine2FormResults r1 r2, [x1, x2], enc)

-- Part 2: combine n forms

data First = First
data Second = Second

instance a ~ a' => Apply First (a, b, c) a' where
  apply _ (a,_,_) = a
instance b ~ b' => Apply Second (a, b, c) b' where
  apply _ (_,b,_) = b

data Deform = Deform

instance Apply Deform (GForm s m xml a) (FormInner s m (FormResult a, xml, Enctype)) where
  apply _ (GForm f) = f

data CombineFormResults = CombineFormResults

instance Apply CombineFormResults (FormResult e, FormResult l) (FormResult (HCons e l)) where
  apply _ (FormSuccess e, FormSuccess l) = FormSuccess $ HCons e l
  apply _ (FormMissing, FormMissing) = FormMissing
  apply _ (f1,f2) = FormFailure $ formFailure f1 ++ formFailure f2

tupleFormResult :: Tuple l t => FormResult l -> FormResult t
tupleFormResult (FormSuccess l) = FormSuccess $ tuple l
tupleFormResult (FormFailure fs) = FormFailure fs
tupleFormResult FormMissing = FormMissing

class CombineForms s y t xml rt | t -> rt, t -> xml where
  combineForms :: t -> GForm s y [xml] rt

instance (Untuple t l
          -- get form monadic actions
         ,HMap Deform l as

          -- sequence form actions
         ,HSequence as (FormInner s y vs)

          -- extract hlist of FormResults from the form action values
         ,HMap First vs rs

          -- combine hlist of FormResults into FormResult hlist
         ,HFoldr CombineFormResults (FormResult HNil) rs (FormResult rl)
          -- tuple FormResult hlist into FormResult tuple
         ,Tuple rl rt

          -- combine widgets
         ,HMapOut Second vs xml

          -- get enctype. NOTE: assumes (without checking) Enctype will be the same for all combined forms
         ,HHead vs (unused1,unused2,Enctype)
         ) => CombineForms s y t xml rt where
  combineForms t = GForm $ do
    let l = untuple t
        as = hMap Deform l
    vs <- hSequence as
    let rs = hMap First vs
        rl = hFoldr CombineFormResults (FormSuccess HNil) rs
        rt = tupleFormResult rl
        ws = hMapOut Second vs
        (_,_,e) = hHead vs
    return $ (rt,ws,e)

