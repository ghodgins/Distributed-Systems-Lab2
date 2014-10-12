module Main where
import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.List.Split

maxThreadCount = 2

main:: IO ()
main = withSocketsDo $ do
	args <- getArgs
	let port = head args

	addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
	let serveraddr = head addrinfos
	sock <- socket (addrFamily serveraddr) Stream defaultProtocol
	setSocketOption sock ReuseAddr 1
	bindSocket sock (addrAddress serveraddr)
	listen sock 5

	putStrLn $ "Listening on " ++ (head args)

	chan <- newChan
	threadCount <- atomically $ newTVar 0

	forkIO $ sockHandler sock chan threadCount

	mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do
	chanMsg <- readChan chan

	case (chanMsg) of
		("KILL_SERVICE") -> putStrLn "Service is now terminating!"
		_ -> mainHandler sock chan

sockHandler :: Socket -> Chan String -> TVar Int -> IO ()
sockHandler sock chan threadCount = do
	(s, addr) <- accept sock
	handle <- socketToHandle s ReadWriteMode
	hSetBuffering handle NoBuffering

	count <- atomically $ readTVar threadCount
	putStrLn $ "threadCount = " ++ show count

	if (count < maxThreadCount) then do
		forkFinally (commandProcessor handle chan addr threadCount) (\_ -> atomically $ decrementTVar threadCount)
		atomically $ incrementTVar threadCount
		else do
			hPutStrLn handle "Service reached maximum capacity, please try again later!"
			hClose handle

	sockHandler sock chan threadCount

commandProcessor :: Handle -> Chan String -> SockAddr -> TVar Int -> IO ()
commandProcessor handle chan addr threadCount = do
	line <- hGetLine handle
	let cmd = words line

	case (head cmd) of
		("HELO") -> heloCommand handle chan addr threadCount (unwords (tail cmd))
		("KILL_SERVICE") -> killCommand handle chan
		_ -> do hPutStrLn handle ("Unknown Command - " ++ line)
	commandProcessor handle chan addr threadCount

heloCommand :: Handle -> Chan String -> SockAddr -> TVar Int -> String -> IO ()
heloCommand handle chan (SockAddrInet addrIP addrPort) threadCount msg = do
	writeChan chan "HELO command processed!"

	hPutStrLn handle $	"HELO "++msg++"\n\
	                 	\IP: " ++ show addrIP ++ "\n\
	                 	\Port: " ++ show addrPort ++ "\n\
	                 	\StudentID:11396966"

killCommand :: Handle -> Chan String -> IO ()
killCommand handle chan = do
	hPutStrLn handle "Service is now terminating!"
	writeChan chan "KILL_SERVICE"

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)