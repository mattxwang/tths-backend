{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status
import Web.Scotty

import Lib

helloAction :: ActionM ()
helloAction = do text "hello world!"

typecheckAction :: ActionM ()
typecheckAction = do
  code <- param "input" `rescue` (const next)
  r <- typecheckExpression code
  case r of
    Left err -> do
      putError err
      json $ object [ "error" .= ("500" :: String) ]
      status internalServerError500
    Right resp ->  do
      json $ object [ "signature" .= (resp :: String) ]

typecheckFailure :: ActionM ()
typecheckFailure = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  status badRequest400

routes :: ScottyM ()
routes = do
  get "/hello" helloAction
  post "/typecheck" typecheckAction
  post "/typecheck" typecheckFailure

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 routes
