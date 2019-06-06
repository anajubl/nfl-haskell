{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Network.HTTP.Types.Status
import Text.Lucius
import Text.Julius


formLogin :: Form (Text,Text)
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    (widgetForm, enctype) <- generateFormPost formLogin
   
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
        toWidget $(luciusFile "templates/login.lucius")
      
    
postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [UsuarioEmail ==. email,
                                          UsuarioSenha ==. senha] []
            case logado of
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (pack $ show usuario)
                    
                    redirect AdminR
                Nothing -> do 
                    
                    redirect LoginR
        _ -> redirect LoginR
        


        
