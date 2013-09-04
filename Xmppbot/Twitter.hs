{-# LANGUAGE OverloadedStrings #-}
module Xmppbot.Twitter where

import           Codec.Binary.UTF8.String (decodeString, encodeString)
import           Control.Applicative ((<|>), many)
import qualified Control.Exception as E
import           Control.Monad (forM)
import           Data.Attoparsec.Char8
import           Data.Attoparsec.Combinator (manyTill)
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (hLocation)

isURI :: C.ByteString -> Bool
isURI u = "http://" `C.isPrefixOf` u || "https://" `C.isPrefixOf` u

expandShortUrl :: String -> IO String
expandShortUrl tweet =
    case parseTweet tweet of
        Right pieces' -> do
            expanded <- forM pieces' $ \piece ->
                if isURI piece then expand piece 
                               else return piece
            return $ unwords $ map (decodeString . C.unpack) expanded
        Left err -> return tweet
  where
    parseTweet t = parseOnly tweetParser (C.pack $ encodeString t)

expand :: C.ByteString -> IO C.ByteString
expand piece = do
    E.catch (do req <- parseUrl $ C.unpack piece
                withManager $ \manager -> 
                    httpLbs req { redirectCount = 0 } manager >> return piece)
            (\e -> case e of
                (StatusCodeException s hdr _) -> do
                    uri <- redirect hdr 
                    -- Sometimes, the location header has no host name!
                    if isURI uri then expand uri else return piece
                otherException -> print otherException >> return piece
            )
  where
    redirect hdr = case HM.lookup hLocation $ HM.fromList hdr of 
                        Just url -> return url
                        Nothing  -> return piece

uriParser :: C.ByteString -> Parser [C.ByteString]
uriParser scheme = do
    v <- manyTill anyChar (try (string scheme))
    link <- takeTill $ notInClass "0-9a-zA-z.:/"
    return [C.pack v, C.append scheme link]

tweetParser :: Parser [C.ByteString]
tweetParser = do
    piece <- many (uriParser "http://" <|> uriParser "https://")
    rest <- takeByteString
    return $ concat piece ++ [rest]
