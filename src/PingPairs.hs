module Main where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Network.Linx.Gateway
import System.Environment (getArgs)

type GWAddress = (String, Int)
type DelaySpec = [(Int, Int)]

-- | The PingPairs program is establishing N number of "ping pairs".
main :: IO ()
main = do
  args <- getArgs
  case args of
    [gateway, gatewayPort, pairs, delaySpec] ->
      runPingPairs (gateway, (read gatewayPort))
                   (read pairs) 
                   (read delaySpec)
    _                                             -> help
      

runPingPairs :: GWAddress -> Int -> DelaySpec -> IO ()
runPingPairs gwAddress num delaySpec = do
  let delays = expandDelaySpec delaySpec
  r <- mapConcurrently (pingPair gwAddress delays) [1..num]
  print r

pingPair :: GWAddress -> [Int] -> Int -> IO [Int]
pingPair gwAddress delays myId =
  withAsync (pingServer gwAddress myId) (pingClient gwAddress delays myId)
  
pingServer :: GWAddress -> Int -> IO ()
pingServer gwAddress myId = return ()

pingClient :: GWAddress -> [Int] -> Int -> Async () -> IO [Int]
pingClient gwAddress delays myId _ = return [myId]

serverId :: Int -> String
serverId n = "server" ++ (show n)

clientId :: Int -> String
clientId n = "client" ++ (show n)
           
expandDelaySpec :: DelaySpec -> [Int]
expandDelaySpec = concat . map (uncurry replicate)

help :: IO ()
help = do
  putStrLn "Usage: PingPairs <gateway address> <gateway port> <pairs> <freqs>"
  putStrLn " <gateway address>: IP address to gateway. E.g. 192.168.122.8"
  putStrLn " <gateway port>   : Port to gateway. E.g. 21768"
  putStrLn " <pairs>          : Communicating pairs. E.g. 7"
  putStrLn " <freqs>          : Ping intensity. E.g. \"[(10, 1000), (100, 100)]\""