{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Binary
import Data.Time (getCurrentTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Network.Linx.Gateway
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as LBS
import GHC.Int

type GWAddress = (HostName, String)

#define HuntSig (SigNo 1)
#define SetupRequestSig (SigNo 10)
#define SetupReplySig (SigNo 11)
#define BulkSessionRequestSig (SigNo 12)
#define BulkSessionReplySig (SigNo 13)
#define BulkSessionCompleteSig (SigNo 14)
#define BulkSig (SigNo 15)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", gateway, port]                 -> 
      runServer (gateway, port)
    ["client", gateway, port, pairs, sigSize] ->
      print =<< runClient (gateway, port) (read pairs) (read sigSize)
    _                                         ->
      putStrLn "Bla bla"
            
runServer :: GWAddress -> IO ()
runServer gwAddress@(ip, port) = 
  bracket (create "server" ip (Service port)) destroy handleSetupRequest  
  where
    handleSetupRequest :: Gateway -> IO ()
    handleSetupRequest gw =
      forever $ do
        (from, Signal SetupRequestSig lbs) <- receive gw $ Sel [SetupRequestSig]
        let numServers = decodeSetupRequest lbs
            names      = map ("server" #) [1..numServers]
        servers <- mapM (async . bulkServer gwAddress) names
        sendWithSelf gw from $ Signal SetupReplySig (encodeSetupReply names)
        mapM_ wait servers
        
bulkServer :: GWAddress -> String -> IO ()
bulkServer (ip, port) name =
  bracket (create name ip (Service port)) destroy handleBulkSession
  where
    handleBulkSession :: Gateway -> IO ()
    handleBulkSession gw = do
      (from, Signal BulkSessionRequestSig lbs) <- 
        receive gw $ Sel [BulkSessionRequestSig]
      putStrLn $ "Request for: " ++ show (decode lbs :: Int64)
      sendWithSelf gw from $ NumericSignal BulkSessionReplySig
      gw `bulkReceive` decode lbs
      sendWithSelf gw from $ NumericSignal BulkSessionCompleteSig
      
    bulkReceive :: Gateway -> Int64 -> IO ()
    bulkReceive gw left
      | left <= 0 = return ()
      | otherwise = do
        (_, Signal BulkSig bulk) <- receive gw $ Sel [BulkSig]
        putStrLn $ "Receive chunk of size: " ++ show (LBS.length bulk)
        gw `bulkReceive` (left - LBS.length bulk)

runClient :: GWAddress -> Int -> Int64 -> IO [NominalDiffTime]
runClient gwAddress@(ip, port) pairs sigSize = 
  bracket (create "client" ip (Service port)) destroy go
  where
    go :: Gateway -> IO [NominalDiffTime]
    go gw = do
      serverNames <- serverNamesFromSetup gw
  --    print serverNames
      let clientNames = map ("client" #) [1..pairs]
      mapConcurrently (bulkClient gwAddress sigSize) 
                      $ zip clientNames serverNames
    
    serverNamesFromSetup :: Gateway -> IO [String]
    serverNamesFromSetup gw = do
      pid <- connectService gw "server"
      sendWithSelf gw pid $ Signal SetupRequestSig (encodeSetupRequest pairs)
      (_, Signal SetupReplySig lbs) <- receive gw $ Sel [SetupReplySig]
      return $ decodeSetupReply lbs

bulkClient :: GWAddress -> Int64 -> (String, String) -> IO NominalDiffTime
bulkClient (ip, port) sigSize (me, server) =
  bracket (create me ip (Service port)) destroy handleBulkSession
  where
    handleBulkSession :: Gateway -> IO NominalDiffTime
    handleBulkSession gw = do
      let bulk = LBS.replicate bulkSize 65
--      print "Try connect server ..."
      pid <- gw `connectService` server
--      print "Done"
      sendWithSelf gw pid $ Signal BulkSessionRequestSig (encode bulkSize)
      _ <- receive gw $ Sel [BulkSessionReplySig]
--      print "Got bulk session reply ..."
      start <- getCurrentTime
      sendBulk gw pid bulk
      _ <- receive gw $ Sel [BulkSessionCompleteSig]
      stop <- getCurrentTime
      return $ diffUTCTime stop start
      
    sendBulk :: Gateway -> Pid -> LBS.ByteString -> IO ()
    sendBulk gw pid bulk
      | LBS.null bulk = return ()
      | otherwise = do
        let (b, bs) = LBS.splitAt sigSize bulk
        sendWithSelf gw pid $ Signal BulkSig b
--        print "Sent b ..."
        sendBulk gw pid bs

connectService :: Gateway -> String -> IO Pid
connectService gw service = do
  _ <- hunt gw service $ NumericSignal HuntSig
  fst <$> receive gw (Sel [HuntSig])

(#) :: String -> Int -> String
(#) service num = service ++ show num      

decodeSetupRequest :: LBS.ByteString -> Int
decodeSetupRequest = decode

encodeSetupRequest :: Int -> LBS.ByteString
encodeSetupRequest = encode

decodeSetupReply :: LBS.ByteString -> [String]
decodeSetupReply = decode

encodeSetupReply :: [String] -> LBS.ByteString
encodeSetupReply = encode

bulkSize :: Int64
bulkSize = 1 * megabyte

megabyte :: Int64
megabyte = 1000000