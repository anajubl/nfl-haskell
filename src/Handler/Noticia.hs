{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Noticia where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

getNoticiaR :: Handler Html
getNoticiaR = do
    
    noticias <- runDB $ selectList [] [Asc NoticiaId]

    defaultLayout $ do 
        
        addStylesheet $ StaticR css_style_css
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "https://use.fontawesome.com/releases/v5.8.2/css/all.css"
        $(whamletFile "templates/noticia.hamlet")
        toWidget $(luciusFile "templates/noticia.lucius")

formNoticia :: Form Noticia
formNoticia = renderBootstrap $ Noticia
    <$>areq (selectField listaTimes) "Lista de Times: " Nothing
    <*> areq textField "Titulo: " Nothing
    <*> areq textField "Descrição: " Nothing
    <*> areq textField "Texto: " Nothing

listaTimes = do
       entidades <- runDB $ selectList [] [Asc TimeNome] 
       optionsPairs $ fmap (\ent -> (timeNome $ entityVal ent, entityKey ent)) entidades


getNoticiafR :: Handler Html
getNoticiafR = do

    (widgetForm, enctype) <- generateFormPost formNoticia
    defaultLayout $ do 
        
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/noticiaf.hamlet")
        toWidget $(luciusFile "templates/noticiaf.lucius")
        

postNoticiaR :: Handler Html
postNoticiaR = do 
    ((res,_),_) <- runFormPost formNoticia
    case res of 
        FormSuccess noticia -> do 
            runDB $ insert noticia 
            redirect NoticiaR
        _ -> redirect NoticiafR


-- postDeletarNoticiaR :: NoticiaId -> Handler Html
-- postDeletarNoticiaR notid = do 
--     _ <- runDB $ get404 notid
--     runDB $ delete notid
--     redirect NoticiaR
