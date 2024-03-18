{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

module Twitch.IRC where

import Control.Concurrent
    ( ThreadId
    , forkIO
    , killThread )

import Control.Concurrent.STM
    ( TChan
    , atomically
    , newTChanIO
    , readTChan )

import Control.Exception
    ( IOException
    , catch )

import Control.Monad
    ( forever )

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
    , sendLazy )

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL


-- Types

type Connection = (Context, SockAddr)

type LineF = BL.ByteString -> IO ()

data Config = Config
    { channels :: [BL.ByteString] }
    deriving Show

data Operation =
      Join [BL.ByteString]
    | Part [BL.ByteString]
    deriving Show

-- Misc

(+++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
(+++) = BL.append

swapState :: MonadState s m => s -> m s
swapState x = get >>= \s -> put x >> return s

hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState

logInfo :: BL.ByteString -> IO ()
logInfo = BL.putStr . (+++) "(info) "

logError :: IOException -> IO ()
logError e = BL.putStr "(error) " >> (putStrLn $ show e)

logError' :: BL.ByteString -> IO ()
logError' = BL.putStr . (+++) "(error) "

sendLazyLogging :: Context -> BL.ByteString -> IO ()
sendLazyLogging ctx x = (logInfo $ "-> " +++ x) >> sendLazy ctx x

-- String splitting

consume :: [B.ByteString] -> State Builder [BL.ByteString]
consume [] = return []
consume [x] = modify (\buf -> buf <> (byteString x)) >> return []
consume (x:xs) = do
    line <- toLazyByteString . (flip (<>) (byteString x)) <$> swapState mempty
    return . (:) line =<< consume xs

feed :: B.ByteString -> State Builder [BL.ByteString]
feed = consume . split "\r\n"

-- Connection

join :: Connection -> [BL.ByteString] -> IO ()
join _ [] = return ()
join (ctx, _) xs = sendLazyLogging ctx $ "JOIN " +++ (BL.intercalate "," xs) +++ "\r\n"

part :: Connection -> [BL.ByteString] -> IO ()
part _ [] = return ()
part (ctx, _) xs = sendLazyLogging ctx $ "PART " +++ (BL.intercalate "," xs) +++ "\r\n"

handleOperation :: Connection -> Operation -> IO ()
handleOperation c (Join cs) = join c cs
handleOperation c (Part cs) = part c cs

processOperations :: Connection -> TChan Operation -> IO ()
processOperations c opsChan = do
    ops <- atomically $ readTChan opsChan
    flip catch logError $ do    -- could fail if socket is closed in the other thread
        handleOperation c ops
        processOperations c opsChan

processLines :: Connection -> (Connection -> LineF) -> StateT Builder (MaybeT IO) ()
processLines c@(ctx, _) f = do
    chunk <- recv ctx >>= lift . hoistMaybe    -- breaks on EOF
    (hoistState $ feed chunk) >>= lift . mapM_ (lift . f c)
    processLines c f

authenticate :: Connection -> IO ()
authenticate (ctx, _) = do
    sendLazyLogging ctx "CAP REQ :twitch.tv/commands twitch.tv/membership twitch.tv/tags\r\n"
    sendLazy ctx "NICK justinfan123\r\n" >> logInfo "-> NICK *\r\n"
    sendLazy ctx "PASS oauth:pass123\r\n" >> logInfo "-> PASS *\r\n"

withTwitchIRC :: (Connection -> IO a) -> IO a
withTwitchIRC f = do
    params <- newDefaultClientParams ("irc.chat.twitch.tv", "6697")
    connect params "irc.chat.twitch.tv" "6697" f

handleLine :: LineF -> Connection -> BL.ByteString -> IO ()
handleLine f (ctx, _) x
    | x == "PING :tmi.twitch.tv" = f x >> sendLazyLogging ctx "PONG :tmi.twitch.tv\r\n"
    | otherwise = f x

-- Main

runTwitchIRC' :: Config -> TChan Operation -> LineF -> IO ()
runTwitchIRC' config opsChan f = withTwitchIRC $ \c -> do
    logInfo "Connected!"
    authenticate c
    join c config.channels
    opsThread <- forkIO $ forever $ processOperations c opsChan
    _ <- runMaybeT $ flip runStateT mempty $ processLines c $ handleLine f
    killThread opsThread
    logError' "*EOF*"

runTwitchIRC :: Config -> LineF -> IO (ThreadId, TChan Operation)
runTwitchIRC config f = do
    opsChan <- newTChanIO
    thread <- forkIO $ runTwitchIRC' config opsChan f
    return (thread, opsChan)
