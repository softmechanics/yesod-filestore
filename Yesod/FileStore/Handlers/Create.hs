{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           #-}

module Yesod.FileStore.Handlers.Create where

import Control.Applicative
import qualified Data.FileStore as S
import Yesod
import Yesod.CombineForm

import Yesod.FileStore.Types
import Yesod.FileStore.Internal

data CreateParams = CreateParams
  { cfFileName :: FilePath
  , cfAuthor :: String
  , cfEmail :: String
  , cfDesc :: Textarea
  }

create :: FSCxt y meta => CreateParams -> meta -> Handler y meta ()
create (CreateParams nm an ae d) meta = do
  store <- getFileStore
  let a = S.Author an ae
      d' = unTextarea d
      c' = show meta
  liftIO $ S.create store nm a d' c'

createFormletField :: Maybe CreateParams -> GForm s y [FieldInfo s y] CreateParams
createFormletField params = CreateParams
  <$> stringField "File Name" (cfFileName <$> params)
  <*> stringField "Author Name" (cfAuthor <$> params)
  <*> emailField "Author Email" (cfEmail <$> params)
  <*> textareaField "Description" (cfDesc <$> params)

getCreateR :: FSCxt y meta => Handler y meta RepHtml
getCreateR = postCreateR

postCreateR :: FSCxt y meta => Handler y meta RepHtml
postCreateR = do
  render <- getFormRenderer
  formletField <- getFormletField
  (res,fis,enc,nonce) <- runFormPost $ combineForms $
    ( createFormletField Nothing
    , formletField Nothing
    )
  case res of
       FormSuccess (ps,m) -> do
         create ps m
         route <- tagToMaster $ ReadT (cfFileName ps)
         redirectParams RedirectTemporary route [("msg", "Saved")]
       _ -> return ()
  defaultLayout $ do
    let form = render fis
    [$hamlet|
<form enctype=#{enc} method=POST
  #{nonce}
  <table
    ^{form}
    <tr
      <td colspan=2
        <input type=submit value="Create"
|]


