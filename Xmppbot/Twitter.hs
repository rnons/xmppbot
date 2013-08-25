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

tco, stco :: C.ByteString
tco = "http://t.co/"
stco = "https://t.co/"

expandShortUrl :: String -> IO String
expandShortUrl tweet = do
    let pieces = parseOnly tweetParser (C.pack $ encodeString tweet)
    lurls <- case pieces of
        Right pieces' -> do
            forM pieces' $ \piece ->
                if (tco `C.isPrefixOf` piece)
                    then expand piece >>= return
                    else
                        return piece
        Left err -> return []
    return $ unwords $ map (decodeString . C.unpack) lurls

expand :: C.ByteString -> IO C.ByteString
expand piece = do
    req <- parseUrl $ C.unpack piece
    E.catch (withManager $ \manager -> 
                httpLbs req { redirectCount = 0 } manager >> return "")
            (\(StatusCodeException s hdr _) -> do
                case HM.lookup hLocation $ HM.fromList hdr of
                    Just url -> return url
                    Nothing  -> return "")

tcoParser :: Parser [C.ByteString]
tcoParser = do
    v <- manyTill anyChar (try (string tco <|> string stco))
    link <- takeTill isSpace
    return $ [C.pack v, C.append tco link]

tweetParser :: Parser [C.ByteString]
tweetParser = do
    piece <- many tcoParser
    rest <- takeByteString
    return $ concat piece ++ [rest]
