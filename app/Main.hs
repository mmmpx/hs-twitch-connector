{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Lazy
    ( MonadState(..)
    , State
    , StateT(runStateT)
    , modify
    , runState )

import Control.Monad.Trans.Class
    ( lift )

import Control.Monad.Trans.Maybe
    ( MaybeT(runMaybeT)
    , hoistMaybe )

import Data.ByteString
    ( ByteString )

import Data.ByteString.Builder
    ( Builder
    , byteString
    , toLazyByteString )

import Data.ByteString.Search
    ( split )

import Network.Simple.TCP.TLS
    ( Context
    , SockAddr
    , connect
    , newDefaultClientParams
    , recv
    , send )

import qualified Data.ByteString.Lazy as BL


-- Types

type Connection = (Context, SockAddr)

-- Misc

alterState :: MonadState s m => (s -> (s, a)) -> m a
alterState f = (f <$> get) >>= (\(s, x) -> put s >> return x)

hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState

-- String splitting

consume :: [ByteString] -> State Builder [BL.ByteString]
consume [] = return []
consume [x] = modify (\buf -> buf <> (byteString x)) >> return []
consume (x:xs) = do
    line <- alterState (\buf -> (mempty, toLazyByteString $ buf <> (byteString x)))
    (return . (:) line) =<< consume xs

feed :: ByteString -> State Builder [BL.ByteString]
feed x = consume $ split "\r\n" x

-- Connection

loopLines :: Connection -> (Connection -> BL.ByteString -> IO ()) -> StateT Builder (MaybeT IO) ()
loopLines c@(ctx, _) f = do
    chunk <- recv ctx >>= lift . hoistMaybe    -- breaks on EOF
    (hoistState $ feed chunk) >>= lift . mapM_ (lift . f c)
    loopLines c f

authenticate :: Connection -> IO ()
authenticate (ctx, _) = do
    send ctx "NICK justinfan123\r\n"
    send ctx "PASS oauth:pass123\r\n"

withTwitchIRC :: (Connection -> IO a) -> IO a
withTwitchIRC f = do
    params <- newDefaultClientParams ("irc.chat.twitch.tv", "")
    connect params "irc.chat.twitch.tv" "6697" f

handleLine :: (BL.ByteString -> IO ()) -> Connection -> BL.ByteString -> IO ()
handleLine f (ctx, _) x
    | x == "PING :tmi.twitch.tv" =
        send ctx "PONG :tmi.twitch.tv\r\n"
            >> f x
            >> putStrLn "[PONG :tmi.twitch.tv]"
    | otherwise = f x

-- Main

main :: IO ()
main = withTwitchIRC $ \c -> do
    authenticate c
    _ <- runMaybeT $ flip runStateT mempty $ loopLines c $ handleLine print
    return ()
