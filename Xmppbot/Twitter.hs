{-# LANGUAGE OverloadedStrings #-}
module Xmppbot.Twitter where

import           Codec.Binary.UTF8.String (decodeString, encodeString)
import           Control.Applicative ((<|>), many)
import qualified Control.Exception as E
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (hLocation)
import           Network.HTTP.Types.Status (status301, status302)

expandShortUrl :: String -> IO String
expandShortUrl tweet =
    case parseTweet tweet of
        Right pieces -> mapM expand pieces >>= merge
        Left _ -> return tweet
  where
    parseTweet t = parseOnly tweetParser (C.pack $ encodeString t)
    merge = return . unwords . map (decodeString . C.unpack)

expand :: C.ByteString -> IO C.ByteString
expand piece
    | isURI piece = E.catch
        (do req <- parseUrl $ C.unpack piece
            withManager $ \manager -> 
                http req { redirectCount = 0 } manager >> return piece)
        (\e -> case e of
            (StatusCodeException s hdr _) -> 
                if s `elem` [status301, status302]
                    then expand (redirect hdr)
                    else return piece
            otherException -> print otherException >> return piece
        )
    | otherwise = return piece
  where
    isURI u = "http://" `C.isPrefixOf` u || "https://" `C.isPrefixOf` u
    redirect hdr = fromMaybe piece (HM.lookup hLocation $ HM.fromList hdr)

uriParser :: C.ByteString -> Parser [C.ByteString]
uriParser scheme = do
    v <- manyTill anyChar (try (string scheme))
    link <- takeTill $ notInClass "0-9a-zA-z"
    return [C.pack v, C.append scheme link]

tweetParser :: Parser [C.ByteString]
tweetParser = do
    piece <- many (uriParser "http://t.co/" <|> uriParser "https://t.co/")
    rest <- takeByteString
    return $ concat piece ++ [rest]
