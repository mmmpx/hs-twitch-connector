{-# LANGUAGE OverloadedStrings #-}

module Twitch.IRC where

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

swapState :: MonadState s m => s -> m s
swapState x = get >>= \s -> put x >> return s

hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState

-- String splitting

consume :: [ByteString] -> State Builder [BL.ByteString]
consume [] = return []
consume [x] = modify (\buf -> buf <> (byteString x)) >> return []
consume (x:xs) = do
    line <- toLazyByteString . (flip (<>) (byteString x)) <$> swapState mempty
    return . (:) line =<< consume xs

feed :: ByteString -> State Builder [BL.ByteString]
feed = consume . split "\r\n"

-- Connection

loopLines :: Connection -> (Connection -> BL.ByteString -> IO ()) -> StateT Builder (MaybeT IO) ()
loopLines c@(ctx, _) f = do
    chunk <- recv ctx >>= lift . hoistMaybe    -- breaks on EOF
    (hoistState $ feed chunk) >>= lift . mapM_ (lift . f c)
    loopLines c f

authenticate :: Connection -> IO ()
authenticate (ctx, _) = do
    send ctx "CAP REQ :twitch.tv/commands twitch.tv/membership twitch.tv/tags\r\n"
    send ctx "NICK justinfan123\r\n"
    send ctx "PASS oauth:pass123\r\n"

withTwitchIRC :: (Connection -> IO a) -> IO a
withTwitchIRC f = do
    params <- newDefaultClientParams ("irc.chat.twitch.tv", "6697")
    connect params "irc.chat.twitch.tv" "6697" f

handleLine :: (BL.ByteString -> IO ()) -> Connection -> BL.ByteString -> IO ()
handleLine f (ctx, _) x
    | x == "PING :tmi.twitch.tv" =
           send ctx "PONG :tmi.twitch.tv\r\n"
        >> f x
        >> putStrLn "[i] PING :tmi.twitch.tv -> PONG :tmi.twitch.tv"
    | otherwise = f x

-- Main

runTwitchIRC :: (BL.ByteString -> IO ()) -> IO ()
runTwitchIRC f = withTwitchIRC $ \c -> do
    putStrLn "[i] Connected!"
    authenticate c
    _ <- runMaybeT $ flip runStateT mempty $ loopLines c $ handleLine f
    putStrLn "[e] *EOF*"
