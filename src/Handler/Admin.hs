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
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/admin.hamlet")
        toWidget $(luciusFile "templates/admin.lucius")
        
        
