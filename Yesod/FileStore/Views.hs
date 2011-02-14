{-# LANGUAGE QuasiQuotes #-}
module Yesod.FileStore.Views where

import Data.FileStore
import Yesod

import Yesod.FileStore.Types
import Yesod.FileStore.Internal

layout :: FSCxt y meta => Layout y meta
layout w = defaultLayout $ do
  addCassius [$cassius|
    .defaultFSView label
      font-weight: bold
      float:left
      padding-right:10px
      width:30%
    .defaultFSView table, .defaultFSView tr, .defaultFSView td
      spacing: 0px;
      padding: 0px;
      margin: 0px;
      border: 0px;
    .defaultFSView th
      vertical-align: top;
      text-align: left;
    th.defaultFSViewFieldSet
      background-color: lightblue;
      text-align: center;
  |]
  w

authorView :: FSCxt y meta => AuthorView y meta
authorView a = addHamlet [$hamlet|
  <tr
    <th> Author Name
    <td> #{authorName a}
  <tr 
    <th> Author Email
    <td> #{authorEmail a}
  |]

revisionView :: FSCxt y meta => RevisionView y meta
revisionView r = do
  authV <- getAuthorView
  authB <- extractBody $ authV $ revAuthor r
  
  addHamlet [$hamlet|
    <tr
      <th> Revision ID
      <td> #{revId r}
    <tr
      <th> Modified
      <td> #{show $ revDateTime r}
    <tr
      <td colspan=2> ^{authB}
  |]

contentView :: FSCxt y meta => ContentView y meta
contentView = addHtml . string . show

readView :: FSCxt y meta => ReadView y meta
readView fp rev con = do
  revV <- getRevisionView
  revB <- extractBody $ revV rev

  conV <- getContentView
  conB <- extractBody $ conV con

  addHamlet [$hamlet|
    <div id=fsRead class=defaultFSView
      <table valign=top
        <tr
          <th colspan=2 class=defaultFSViewFieldSet 
            #{fp}
        ^{revB}
        <tr
          <th colspan=2 class=defaultFSViewFieldSet 
            Contents
        <tr
          <td colspan=2 
            ^{conB}
    |]

