{-# LANGUAGE RecordWildCards #-}

module Wallpapers where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar_, newEmptyMVar, newMVar)
import Control.Concurrent.MVar (takeMVar)
import qualified Control.Foldl as Fold
import DBus
import DBus.Client
import qualified GI.Notify as Notify
import System.FilePath (takeExtension)
import System.Random
import System.Random.Shuffle
import Turtle hiding (export)

data PomoState = Work | ShortBreak | LongBreak
data ThreadState = ThreadState {inPomo :: Bool, threadId :: ThreadId, files :: [FilePath]}

times :: [PomoState]
times = cycle [Work, ShortBreak, Work, ShortBreak, Work, ShortBreak, Work, LongBreak]

stateTime :: PomoState -> NominalDiffTime
stateTime Work = 20 * 60
stateTime ShortBreak = 5 * 60
stateTime LongBreak = 15 * 60

notifyPomo :: PomoState -> IO ()
notifyPomo Work = notify "work"
notifyPomo ShortBreak = notify "short break"
notifyPomo LongBreak = notify "long break"

notify :: Text -> IO ()
notify message = Notify.notificationShow =<< Notify.notificationNew message Nothing Nothing

display :: FilePath -> IO ()
display file =
    procs
        "swww"
        ["img", format fp file, "--transition-fps", "140", "--transition-type", "center"]
        empty

displayPomo :: FilePath -> PomoState -> IO ()
displayPomo file state =
    display file
        >> notifyPomo state
        >> sleep (stateTime state)

displayNormal :: FilePath -> IO ()
displayNormal file = display file >> sleep (20 * 60)

isImg :: FilePath -> Bool
isImg = (`elem` [".gif", ".png", ".jpg", ".jpeg"]) . takeExtension

toggle :: MVar ThreadState -> IO ()
toggle v = modifyMVar_ v $ \s@(ThreadState{..}) -> do
    killThread threadId
    shuffled <- shuffle' files (length files) <$> newStdGen
    let newJob = if inPomo then mapM_ display shuffled else mapM_ (uncurry displayPomo) (align shuffled)
    newThread <- forkIO newJob
    when inPomo $ notify "leaving pomo"
    pure $ s{inPomo = not inPomo, threadId = newThread}
  where
    align = (`zip` times) . cycle

setupDBus :: ThreadState -> IO ()
setupDBus state = do
    client <- connectSession
    let serviceName = busName_ "org.jbrown.hallpaper"
    -- the options we have here ensure that we don't need to check the result
    -- of the name request, but print result anyway to verify while testing
    requestName
        client
        serviceName
        [nameAllowReplacement, nameReplaceExisting, nameDoNotQueue]
        >>= print
    stateVar <- newMVar state
    export
        client
        "/"
        defaultInterface
            { interfaceName = "org.jbrown.toggle"
            , interfaceMethods =
                [autoMethod "toggle" (toggle stateVar)]
            }

start :: [FilePath] -> IO ThreadState
start files = do
    shuffled <- shuffle' files (length files) <$> newStdGen
    threadId <- forkIO (mapM_ display shuffled)
    pure ThreadState{threadId = threadId, files = files, inPomo = False}

daemonMain :: IO ()
daemonMain = do
    Notify.init (Just "daemon")
    homePath <- home
    let fstream = ls (homePath ++ "/.config/sway/wallpapers")
    fs <- filter isImg <$> fold fstream Fold.list
    state <- start fs
    setupDBus state
    -- somewhat hacky way to make the main thread block indefinitely
    newEmptyMVar >>= takeMVar

clientMain :: IO ()
clientMain = do
    client <- connectSession
    let req =
            (methodCall "/" "org.jbrown.toggle" "toggle")
                { methodCallDestination = Just "org.jbrown.hallpaper"
                }

    reply <- call client req

    -- Handle the reply
    case reply of
        Left error -> putStrLn $ "Error: " ++ show error
        Right result -> putStrLn $ "Received reply: " ++ show result

    disconnect client
