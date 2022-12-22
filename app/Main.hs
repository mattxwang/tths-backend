{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Lib

main :: IO ()
main = do
  someFunc
  putStrLn "Starting Server..."
  scotty 3000 $ do
      get "/hello" $ do
          text "hello world!"
