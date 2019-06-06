{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import

import Text.Lucius
import Text.Julius
import Prelude (read)

getAdminR :: Handler Html
getAdminR = do
    
    noticias <- runDB $ selectList [] [Asc NoticiaId]

    defaultLayout $ do 
        addStylesheet $ StaticR css_style_css
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "https://use.fontawesome.com/releases/v5.8.2/css/all.css"
        $(whamletFile "templates/admin.hamlet")
        toWidget $(luciusFile "templates/admin.lucius")
      
     
      
formNoticiaUpdate :: TimeId -> Text -> Text -> Text -> Form Noticia
formNoticiaUpdate time titulo descricao texto = renderBootstrap $ Noticia
    <$> areq (selectField listaTimes) "Lista de Times: " (Just time)
    <*> areq textField "Titulo: " (Just titulo)
    <*> areq textField "Descrição: " (Just descricao)
    <*> areq textField "Texto: " (Just texto)
     
     
listaTimes = do
       entidades <- runDB $ selectList [] [Asc TimeNome] 
       optionsPairs $ fmap (\ent -> (timeNome $ entityVal ent, entityKey ent)) entidades
   
        
getNoticiaUpdateR :: NoticiaId -> Handler Html
getNoticiaUpdateR noticiaId = do

    (Noticia time titulo descricao texto) <- runDB $ get404 noticiaId
    ((res, widgetForm ), enctype) <- runFormPost $ formNoticiaUpdate time titulo descricao texto
    case res of
        FormMissing -> defaultLayout $(whamletFile "templates/alterarnoticia.hamlet")
        FormFailure x -> redirect $ NoticiaUpdateR noticiaId
        FormSuccess noticia -> do 
            runDB $ do
                replace noticiaId noticia
            redirect AdminR
            
postNoticiaUpdateR :: NoticiaId -> Handler Html
postNoticiaUpdateR noticiaId = getNoticiaUpdateR noticiaId



postNoticiaDelR :: NoticiaId -> Handler Html
postNoticiaDelR notid = do 
    runDB $ delete notid
    redirect AdminR