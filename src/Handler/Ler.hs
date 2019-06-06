{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Ler where

import Import

import Text.Lucius
import Text.Julius
import Prelude (read)

getLerR :: NoticiaId -> Handler Html
getLerR noticiaid = do
    
    note <- runDB $ get404 noticiaid
    noticias <- runDB $ selectList [ NoticiaId ==. noticiaid] [] 
    defaultLayout $ do 
        
        addStylesheet $ StaticR css_style_css
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "https://use.fontawesome.com/releases/v5.8.2/css/all.css"
        $(whamletFile "templates/ler.hamlet")
        toWidget $(luciusFile "templates/ler.lucius")
        