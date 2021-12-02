{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.List (intercalate)
import qualified Network.Socket as Socket -- $ cabal install network
import           System.Environment
import           System.IO
import           Text.Regex -- $ cabal install regex-compat


defaultPortNumber :: Socket.PortNumber
defaultPortNumber = 2222

htmlFolder :: String
htmlFolder = "public_html"


-- Only GET requests for now.
data HTTPRequest =
    GetRequest { path :: String }
    deriving (Show)

data HTTPResponse =
    HTTPResponse { status :: B.ByteString, body :: B.ByteString }
    deriving (Show)

statusOk, statusBadRequest, statusNotFound :: B.ByteString
statusOk         = "200 OK"
statusBadRequest = "400 Bad Request"
statusNotFound   = "404 Not Found"

badRequestBody, notFoundBody :: B.ByteString
badRequestBody = "<!DOCTYPE html><html><head><title>Bad Request</title></head><body><h1>Bad request.</h1>It could be my fault. When I grow up I want to be a full-featured HTTP server.</body></html>"
notFoundBody   = "<!DOCTYPE html><html><head><title>Not Found</title></head><body><h1>404!</h1>File not found!</body></html>"

byteStringToString :: B.ByteString -> String
byteStringToString = map (toEnum . fromEnum) . B.unpack


-- We log strings as we perform IO over TCP with the client.
type Logger a = WriterT [String] IO a


newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }


-- The type of the logged thing inside a writer is always
-- constrained to be a Monoid for two reasons:
-- (a) gives us a starting element (mempty)
-- (b) ensures associativity of bind
--
-- Analogous to execState, execWriterT runs the
-- writerT and returns the log, although wrapped
-- in the inner monad (see the type).
execWriterT :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriterT writerT = do
    (_, written) <- runWriterT writerT
    return written


-- "Write" something to the log.
tell :: Applicative m => w -> WriterT w m ()
tell thingToWrite = WriterT $ undefined


-- Essentially fmap on the log.
--
-- For an extra challenge, you can relax the typeclass
-- constraint to "Functor m" instead of "Monad m".
censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f writerT = undefined


instance Functor m => Functor (WriterT w m) where
    -- Want to apply f to thing returned by m, not the log.
    fmap :: Functor m => (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f writerTArg = undefined


instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: (Monoid w, Applicative m) => a -> WriterT w m a
    pure a = undefined

    (<*>) :: (Monoid w, Applicative m) => WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    writerTFunc <*> writerTArg =
        -- This is the hardest one. There are several ways to write it.
        -- Don't forget to combine the two logs from writerTFunc and writerTArg!
        undefined


instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: (Monoid w, Monad m) => WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    writerT >>= f = undefined


instance Monoid w => MonadTrans (WriterT w) where
    lift :: (Monoid w, Monad m) => m a -> WriterT w m a
    lift monad = undefined


-- Read the (optional) port number as an argument and then start the listening loop.
--
-- This follows https://wiki.haskell.org/Implement_a_chat_server somewhat.
main :: IO ()
main = Socket.withSocketsDo $ do -- withSocketsDo is supposedly not needed anymore on most systems.
    -- Read port as first argument, or use default.
    args <- getArgs
    let portNumber = if null args
                     then defaultPortNumber
                     else read . head $ args

    -- Now we have to set up data structure explaining to the operating system
    -- what kind of interface we want with the outside world.

    -- We want an internet connection (INET) using TCP (Stream).
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    -- The following recommened option reduces contention for local addresses.
    Socket.setSocketOption sock Socket.ReuseAddr 1
    -- Ask the operating system if we can reserve portNumber for ourselves on
    -- all available network interfaces (0).
    Socket.bind sock (Socket.SockAddrInet portNumber 0)
    -- Start listening for connections, allowing as many queued connections as possible.
    Socket.listen sock Socket.maxListenQueue
    putStrLn $ "Waiting for connections on port " ++ show portNumber ++ "..."
    hFlush stdout
    acceptConnectionsLoop sock


-- Wait for a connection then spin off a new thread of execution to
-- handle the communication, then go back to listening.
acceptConnectionsLoop :: Socket.Socket -> IO ()
acceptConnectionsLoop sock = do
    -- When a peer from elsewhere establishes a connection with us, we
    -- get a new socket just for that connection.
    (connSocket, peerAddress) <- Socket.accept sock
    -- Convert the connection to an IO handle for a more familiar IO interface.
    connHandle <- Socket.socketToHandle connSocket ReadWriteMode
    hSetBuffering connHandle LineBuffering
    putStrLn $ "Connection accepted from " ++ show peerAddress
    hFlush stdout
    _ <- forkIO (handleConn connHandle peerAddress)
    acceptConnectionsLoop sock


-- Do the communication with the client and then output the log lines.
handleConn :: Handle -> Socket.SockAddr -> IO ()
handleConn connHandle peerAddress = do
    logLines <- run $ readAndRespond connHandle
    putStr $ unlines logLines
    hFlush stdout
    where
        run :: Logger () -> IO [String]
        run = execWriterT . censor prependPeerName

        prependPeerName :: [String] -> [String]
        prependPeerName logLines =
            map (prefix ++) logLines
            where
                prefix = "[" ++ show peerAddress ++ "] "


-- Read request headers and try to form some response.
readAndRespond :: Handle -> Logger ()
readAndRespond connHandle = do
    -- Get the request lines in a list e.g.: Just [ "GET / HTTP/1.1", "Host: localhost:2222" ]
    maybeReqLines <- lift $ maybeReadRequestLines connHandle
    case maybeReqLines of
        Nothing       -> tell ["Connection broken."]
        Just reqLines ->
            case maybeParseRequestLines reqLines of
                Nothing      -> doResponse $ HTTPResponse statusBadRequest badRequestBody
                Just request -> do
                        tell ["Requested " ++ path request]
                        response <- lift $ createResponse request
                        doResponse response
    where
        doResponse response = do
            lift $ B.hPut connHandle (serializeResponse response)
            tell [byteStringToString $ status response]
            lift $ hClose connHandle
            tell ["Connection closed."]


-- See e.g.: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Client_request
--
-- Input:
-- GET / HTTP/1.1\r\n
-- Host: localhost:2222\r\n
-- \r\n
--
-- Output:
-- [ "GET / HTTP/1.1", "Host: localhost:2222" ]
--
-- Uses MaybeT monad transformer to avoid littering code with
-- lots of case statments checking for Nothing.
maybeReadRequestLines :: Handle -> IO (Maybe [String])
maybeReadRequestLines connHandle = runMaybeT $ do
    -- Remove either trailing \r (*nix) or leading \n (Windows?)
    line <- clean <$> MaybeT (maybeReadLine connHandle)
    if line == "" then
        return []
    else do
        -- Got one line. Now recurse over the rest of the request.
        rest <- MaybeT (maybeReadRequestLines connHandle)
        return $ line : rest
    where
        clean :: String -> String
        clean = filter (not . (`elem` ("\r\n" :: String)))

        -- hGetLine succeeds or throws an exception. We want to work
        -- with Maybes, not exceptions.
        maybeReadLine :: Handle -> IO (Maybe String)
        maybeReadLine h = (Just <$> hGetLine h) `catch` excepToNothing

        -- Catch exception and convert to a Nothing value.
        excepToNothing :: IOError -> IO (Maybe a)
        excepToNothing _ = return Nothing


-- Only accepts "GET something HTTP/1.1"
maybeParseRequestLines :: [String] -> Maybe HTTPRequest
maybeParseRequestLines []            = Nothing
maybeParseRequestLines (firstLine:_) =
    case words firstLine of
        [ "GET", path, "HTTP/1.1" ] -> Just $ GetRequest path
        _                           -> Nothing


-- Look up the requested file and read it into an HTTPResponse
createResponse :: HTTPRequest -> IO HTTPResponse
createResponse (GetRequest path) = do
    let filePaths = requestPathToFilePaths path
    maybeContents <- maybeReadOneFile filePaths
    return $
        case maybeContents of
            Just fileContents -> HTTPResponse statusOk fileContents
            Nothing           -> HTTPResponse statusNotFound notFoundBody


-- We'll try reading files until one succeeds.
--
-- We have to use ByteString to ensure we send the right content
-- length when there are unicode characters present.
maybeReadOneFile :: [String] -> IO (Maybe B.ByteString)
maybeReadOneFile []          = return Nothing
maybeReadOneFile (path:rest) =
    (Just <$> B.readFile path) `catch` tryNext
    where
        -- Haskell wants to know what kind of exceptions we are
        -- catching, even though it doesn't matter.
        tryNext :: IOError -> IO (Maybe B.ByteString)
        tryNext _ = maybeReadOneFile rest


-- Try both "/file" and "/file/index.html"
-- Convert "/" to "\" for Windows. (I think!)
-- and try to provide a minimum of security by filtering out ".."
requestPathToFilePaths :: String -> [String]
requestPathToFilePaths path =
      let parts = htmlFolder : splitRegex (mkRegex "/|\\\\|\\.\\.") path in
      [ intercalate "/"  parts
      , intercalate "/"  (parts ++ ["index.html"])
      , intercalate "\\" parts
      , intercalate "\\" (parts ++ ["index.html"])
      ]


-- Only sets Content-Type for HTML right now. If you want to serve
-- images or CSS or Javascript you will have to make some modifications.
serializeResponse :: HTTPResponse -> B.ByteString
serializeResponse (HTTPResponse statusCode body) =
    B.concat
        [ "HTTP/1.1 ", statusCode, crlf
        , "Content-Type: text/html; charset=UTF-8", crlf
        , "Content-Length: ", lengthStr, crlf
        , "Server: Lab 7 Silly Haskell Server 1.0", crlf
        , "Connection: close", crlf
        , "", crlf
        , body
        ]
    where
        crlf      = "\r\n" -- HTTP header lines are separated by carriage return plus line feed.
        lengthStr = C.pack . show . B.length $ body

