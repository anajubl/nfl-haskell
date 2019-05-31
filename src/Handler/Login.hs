{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$> areq emailField "Email: " Nothing
    <*> areq passwordField "Confirmacao: " Nothing
    
-- @ -> link
-- ^ -> OUTRO HTML
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    mensagem <- getMessage
    defaultLayout $ do 
        [whamlet|
            $maybe msg <- mensagem
                ^{msg}
            <form action=@{LoginR} method=post>
                ^{widget}
                <input type="submit" value="Entrar">
        |]

postLoginR :: Handler Html
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of
        FormSuccess (email,senha) -> do 
            maybeUsuario <- runDB $ getBy (UniqueEmail email)
            case maybeUsuario of 
                Just (Entity uid usuario) -> do 
                    if (usuarioSenha usuario) == senha then do 
                        setSession "_ID" (usuarioNome usuario)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <h2>
                                Senha incorreta
                        |]
                        redirect LoginR
                Nothing -> do 
                        setMessage [shamlet|
                            <h2>
                                Usuario n encontrado!
                        |]
                        redirect LoginR
        _ -> redirect HomeR

