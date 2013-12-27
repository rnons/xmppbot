{-# LANGUAGE OverloadedStrings #-}
module Xmppbot.Translate
  ( translate
  ) where

import           Data.Attoparsec.ByteString.Lazy
import           Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString as B
import           Network.HTTP.Types
import           Network.HTTP.Conduit

translate :: String -> IO String
translate q = do
    let url = "http://translate.google.com/translate_a/t"
        query = [ ("client", "t")
                , ("sl", "auto")
                , ("tl", "en")
                , ("hl", "en")
                , ("sc", "2")
                , ("ie", "UTF-8")
                , ("oe", "UTF-8")
                , ("uptl", "zh-CN")
                , ("ssel", "0")
                , ("tsel", "0")
                , ("q", fromString q)
                ]
    initReq <- parseUrl url
    let req = initReq { queryString = renderSimpleQuery False query}
    res <- withManager $ \manager -> httpLbs req manager
    case eitherResult $ parse trParser (responseBody res) of
        Right p -> return $ toString p
        Left err -> error err

--trParser :: Parser [B.ByteString, B.ByteString, B.ByteString]
trParser :: Parser B.ByteString
trParser = do
    string "[[["
    piece <- takeTill $ inClass ","
    takeLazyByteString
    return piece



