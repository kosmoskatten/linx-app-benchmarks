{-# LANGUAGE CPP, BangPatterns #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Binary
import Data.Time (getCurrentTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Network.Linx.Gateway
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LBS

type GWAddress = (String, String)
type DelaySpec = [(Int, Int)]

#define HuntSig (SigNo 1)
#define PingSig (SigNo 100)
#define PongSig (SigNo 101)

-- | The PingPairs program is establishing N number of "ping pairs".
main :: IO ()
main = do
  args <- getArgs
  case args of
    [gateway, gatewayPort, pairs, delaySpec] ->
      runPingPairs (gateway, gatewayPort)
                   (read pairs) 
                   (read delaySpec)
    _                                        -> help
      

runPingPairs :: GWAddress -> Int -> DelaySpec -> IO ()
runPingPairs gwAddress num delaySpec = do
  let delays = expandDelaySpec delaySpec
  r <- mapConcurrently (pingPair gwAddress delays) [1..num]
  print r

pingPair :: GWAddress -> [Int] -> Int -> IO [NominalDiffTime]
pingPair gwAddress delays myId =
  withAsync (pingServer gwAddress myId) (pingClient gwAddress delays myId)
  
pingServer :: GWAddress -> Int -> IO ()
pingServer (ip, port) myId =
  bracket (create ("server" # myId) ip (Service port))
          destroy $ \gw ->
    forever $ do
      (from, Signal PingSig lbs) <- receive gw $ Sel [PingSig]
      sendWithSelf gw from $ Signal PongSig lbs    

pingClient :: GWAddress -> [Int] -> Int -> Async () -> IO [NominalDiffTime]
pingClient (ip, port) delays myId _ =
  bracket (create ("client" # myId) ip (Service port))
          destroy $ \gw -> do
    go gw delays [] =<< connectServer gw ("server" # myId)
  where
    go :: Gateway -> [Int] -> [NominalDiffTime] -> Pid -> IO [NominalDiffTime]
    go _ [] result _ = return $ reverse result
    go gw (t:ts) result pid = do
      timestamp <- serializedTimestamp
      sendWithSelf gw pid $ Signal PingSig timestamp
      (_, Signal PongSig lbs) <- receive gw $ Sel [PongSig]
      !rtt <- captureRTT lbs
      if not (null ts) then do
        threadDelay t
        go gw ts (rtt:result) pid
        else go gw [] (rtt:result) pid
      
    connectServer :: Gateway -> String -> IO Pid
    connectServer gw service = do
      _ <- hunt gw service $ NumericSignal HuntSig
      (pid, NumericSignal HuntSig) <- receive gw $ Sel [HuntSig]
      return pid

serializedTimestamp :: IO LBS.ByteString
serializedTimestamp = encode . show <$>  getCurrentTime

captureRTT :: LBS.ByteString -> IO NominalDiffTime
captureRTT lbs = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime $ (read . decode) lbs

(#) :: String -> Int -> String
(#) name number = name ++ (show number)

expandDelaySpec :: DelaySpec -> [Int]
expandDelaySpec = concat . map (uncurry replicate)

help :: IO ()
help = do
  putStrLn "Usage: PingPairs <gateway address> <gateway port> <pairs> <freqs>"
  putStrLn " <gateway address>: IP address to gateway. E.g. 192.168.122.8"
  putStrLn " <gateway port>   : Port to gateway. E.g. 21768"
  putStrLn " <pairs>          : Communicating pairs. E.g. 7"
  putStrLn " <freqs>          : Ping intensity. E.g. \"[(10, 1000), (100, 100)]\""