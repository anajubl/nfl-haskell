{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql
import Text.Lucius

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario 
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "Email: " Nothing
            <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirmacao: " Nothing
    
-- @ -> link
-- ^ -> OUTRO HTML
getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
    mensagem <- getMessage
    defaultLayout $ do 
        [whamlet|
            $maybe msg <- mensagem
                ^{msg}
            <form action=@{UsuarioR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usuario,confirmacao) -> do 
            if (usuarioSenha usuario) == confirmacao then do 
                runDB $ insert usuario
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h2>
                        Senha e a confirmação n batem!
                |]
                redirect UsuarioR
        _ -> redirect HomeR

