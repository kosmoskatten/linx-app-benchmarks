{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently, wait)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Binary
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock (NominalDiffTime)
import Network.Linx.Gateway
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as LBS

type DelaySpecList = [(Int, Int)]

#define HuntSig (SigNo 1)
#define SetupRequestSig (SigNo 10)
#define SetupReplySig (SigNo 11)
#define PingRequestSig (SigNo 12)
#define PingReplySig (SigNo 13)
#define ByeSig (SigNo 14)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", gateway, port]                -> 
      runServer gateway port
    ["client", gateway, port, pairs, delays] -> do
      result <- runClient gateway port (read pairs) (read delays)
      print result
    _                                        -> do
      putStrLn "Bla bla bla"
      putStrLn "Bla bla bla"
      
runServer :: HostName -> String -> IO ()
runServer ip port = 
  bracket (create "server" ip (Service port))
          destroy $ \gw ->
    forever $ do
      (from, Signal SetupRequestSig lbs) <- receive gw $ Sel [SetupRequestSig]
      let numServers = decodeSetupRequest lbs
          names      = serviceNames numServers
      servers <- mapM (async . pingServer ip port) names
      sendWithSelf gw from $ Signal SetupReplySig (encodeSetupReply names)
      mapM_ wait servers

serviceNames :: Int -> [String]
serviceNames n = map ("server" #) [1..n]

pingServer :: HostName -> String -> String -> IO ()
pingServer ip port name =
  bracket (create name ip (Service port)) destroy go
  where
    go :: Gateway -> IO ()
    go gw = do
      (from, signal) <- receive gw $ Sel [PingRequestSig, ByeSig]
      case signal of
        Signal PingRequestSig lbs -> do
          sendWithSelf gw from $ Signal PingReplySig lbs
          go gw
        _                         -> return ()

runClient :: HostName -> String -> Int -> DelaySpecList 
          -> IO [[NominalDiffTime]]
runClient ip port pairs delays = 
  bracket (create "client" ip (Service port))
          destroy $ \gw -> do
    pid <- connectService gw "server"
    sendWithSelf gw pid $ Signal SetupRequestSig (encodeSetupRequest pairs)
    (_, Signal SetupReplySig lbs) <- receive gw $ Sel [SetupReplySig]
    let servers = decodeSetupReply lbs
        clients = map ("client" #) [1..pairs]
        delays' = extractDelaySpecs delays
    mapConcurrently (pingClient ip port delays') $ zip clients servers
    
pingClient :: HostName -> String -> [Int] -> (String, String) 
           -> IO [NominalDiffTime]
pingClient ip port delays (me, server) =
  bracket (create me ip (Service port))
          destroy $ \gw -> do
    pid <- connectService gw server
    go gw pid delays []
  where
    go :: Gateway -> Pid -> [Int] -> [NominalDiffTime] -> IO [NominalDiffTime]
    go gw pid [] result = do
      sendWithSelf gw pid $ NumericSignal ByeSig
      return $ reverse result
    go gw pid (t:ts) result = do
      timestamp <- encodedTimestamp
      sendWithSelf gw pid $ Signal PingRequestSig timestamp
      (_, Signal PingReplySig lbs) <- receive gw $ Sel [PingReplySig]
      rtt <- captureRtt lbs
      threadDelay t
      go gw pid ts (rtt:result)

connectService :: Gateway -> String -> IO Pid
connectService gw service = do
  _ <- hunt gw service $ NumericSignal HuntSig
  fst <$> receive gw (Sel [HuntSig])
  
decodeSetupRequest :: LBS.ByteString -> Int
decodeSetupRequest = decode

encodeSetupRequest :: Int -> LBS.ByteString
encodeSetupRequest = encode

decodeSetupReply :: LBS.ByteString -> [String]
decodeSetupReply = decode

encodeSetupReply :: [String] -> LBS.ByteString
encodeSetupReply = encode

extractDelaySpecs :: DelaySpecList -> [Int]
extractDelaySpecs = concat . map (uncurry replicate)

(#) :: String -> Int -> String
(#) service num = service ++ (show num)

encodedTimestamp :: IO LBS.ByteString
encodedTimestamp = encode . show <$> getCurrentTime

captureRtt :: LBS.ByteString -> IO NominalDiffTime
captureRtt lbs = do
  timestamp <- getCurrentTime
  return $ timestamp `diffUTCTime` (read . decode) lbs
