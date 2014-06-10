{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (async, mapConcurrently, wait)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Binary
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock (NominalDiffTime)
import Data.List (transpose, zip4, unzip4)
import Network.Linx.Gateway
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as LBS

type DelaySpecList = [(Int, Int)]
type GWAddress = (HostName, String)

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
      runServer (gateway, port)
    ["client", gateway, port, pairs, delays] -> do
      let delays' = extractDelaySpecs $ read delays
      result <- calcResult delays' 
                  <$> runClient (gateway, port) (read pairs) delays'
      presentResult result
    _                                        -> do
      putStrLn "Bla bla bla"
      putStrLn "Bla bla bla"
      
presentResult :: [(Int, NominalDiffTime, NominalDiffTime, NominalDiffTime)] 
              -> IO ()
presentResult xs = do 
  print xs
  let (l1, l2, l3, l4) = unzip4 xs
  writeFile "result.csv" (toCSV "Tick" l1)
  appendFile "result.csv" (toCSV "Min" l2)
  appendFile "result.csv" (toCSV "Average" l3)
  appendFile "result.csv" (toCSV "Max" l4)
  
toCSV :: Show a => String -> [a] -> String
toCSV label xs = label ++ "," ++ (toCSV' xs)
  where
    toCSV' :: Show a => [a] -> String
    toCSV' [] = "\n"
    toCSV' (x:[]) = (show x) ++ "\n"
    toCSV' (x:xs) = (show x) ++ "," ++ (toCSV' xs)

calcResult :: [Int] -> [[NominalDiffTime]] 
           -> [(Int, NominalDiffTime, NominalDiffTime, NominalDiffTime)]
calcResult ts xs =
  let xs' = transpose xs
  in zip4 ts (map minimum xs') (map mean xs') (map maximum xs')
  where
    mean :: [NominalDiffTime] -> NominalDiffTime
    mean [] = error "Should not happen"
    mean xs = sum xs / (fromIntegral $ length xs)

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
        servers <- mapM (async . pingServer gwAddress) names
        sendWithSelf gw from $ Signal SetupReplySig (encodeSetupReply names)
        mapM_ wait servers

pingServer :: GWAddress -> String -> IO ()
pingServer (ip, port) name =
  bracket (create name ip (Service port)) destroy handleSignals
  where
    handleSignals :: Gateway -> IO ()
    handleSignals gw = do
      (from, signal) <- receive gw $ Sel [PingRequestSig, ByeSig]
      case signal of
        Signal PingRequestSig lbs -> do
          sendWithSelf gw from $ Signal PingReplySig lbs
          handleSignals gw
        _                         -> return ()

runClient :: GWAddress -> Int -> [Int] -> IO [[NominalDiffTime]]
runClient gwAddress@(ip, port) pairs delays = 
  bracket (create "client" ip (Service port)) destroy go
  where
    go :: Gateway -> IO [[NominalDiffTime]]
    go gw = do
      serverNames <- serverNamesFromSetup gw
      let clientNames = map ("client" #) [1..pairs]
      mapConcurrently (pingClient gwAddress delays) 
                      $ zip clientNames serverNames
    
    serverNamesFromSetup :: Gateway -> IO [String]
    serverNamesFromSetup gw = do
      pid <- connectService gw "server"
      sendWithSelf gw pid $ Signal SetupRequestSig (encodeSetupRequest pairs)
      (_, Signal SetupReplySig lbs) <- receive gw $ Sel [SetupReplySig]
      return $ decodeSetupReply lbs
      
    
pingClient :: GWAddress -> [Int] -> (String, String) 
           -> IO [NominalDiffTime]
pingClient (ip, port) delays (me, server) =
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
      Nothing <- receiveWithTimeout gw (Timeout (fromIntegral t)) AnySignal
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
extractDelaySpecs = concatMap (uncurry replicate)

(#) :: String -> Int -> String
(#) service num = service ++ show num

encodedTimestamp :: IO LBS.ByteString
encodedTimestamp = encode . show <$> getCurrentTime

captureRtt :: LBS.ByteString -> IO NominalDiffTime
captureRtt lbs = do
  timestamp <- getCurrentTime
  return $ timestamp `diffUTCTime` (read . decode) lbs
