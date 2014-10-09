module Main where
import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent

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

	forkIO $ sockHandler sock chan

	chanHandler sock chan

chanHandler :: Socket -> Chan String -> IO ()
chanHandler sock chan = do
	chanMsg <- readChan chan

	case (chanMsg) of
		("KILL_SERVICE") -> putStrLn "Service is now terminating"
		_ -> chanHandler sock chan

sockHandler :: Socket -> Chan String -> IO ()
sockHandler sock chan = do
	(s, addr) <- accept sock
	handle <- socketToHandle s ReadWriteMode
	hSetBuffering handle NoBuffering
	forkIO $ commandProcessor handle chan addr
	sockHandler sock chan

commandProcessor :: Handle -> Chan String -> SockAddr -> IO ()
commandProcessor handle chan addr = do
	line <- hGetLine handle

	let cmd = filter (/='\r') line

	case (cmd) of
		("HELO text") -> heloCommand handle chan addr
		("KILL_SERVICE") -> killCommand handle chan
		_ -> do hPutStrLn handle ("Unknown Command - " ++ line)
	commandProcessor handle chan addr

heloCommand :: Handle -> Chan String -> SockAddr -> IO ()
heloCommand handle chan addr = do
	writeChan chan "HELO command processed!"
	hPutStrLn handle "HELO text\n\
	                 \IP:[ip address]\n\
	                 \Port:8080\n\
	                 \StudentID:11396966"

killCommand :: Handle -> Chan String -> IO ()
killCommand handle chan = do
	hPutStrLn handle "Service is now terminating!"
	writeChan chan "KILL_SERVICE"