{-# LANGUAGE RecordWildCards #-}

module Wallpapers where

import Control.Arrow ((&&&))
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar_, newEmptyMVar, newMVar, readMVar)
import Control.Concurrent.MVar (takeMVar)
import qualified Control.Foldl as Fold
import DBus
import DBus.Client
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified GI.Notify as Notify
import System.FilePath (takeExtension)
import System.Random
import System.Random.Shuffle
import Turtle hiding (export)

data PomoState = Work | ShortBreak | LongBreak
data Pomo = Pomo {state :: PomoState, startTime :: UTCTime}
data ThreadState = ThreadState {pomo :: Maybe Pomo, threadId :: ThreadId, files :: [FilePath], gen :: StdGen}

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

displayPomo :: MVar ThreadState -> FilePath -> PomoState -> IO ()
displayPomo v file state =
    display file
        >> notifyPomo state
        >> modifyMVar_ v setTime
        >> sleep (stateTime state)
  where
    setTime s =
        getCurrentTime
            >>= \t -> pure s{pomo = Just $ Pomo{startTime = t, state = state}}

displayNormal :: FilePath -> IO ()
displayNormal file = display file >> sleep (20 * 60)

isImg :: FilePath -> Bool
isImg = (`elem` [".gif", ".png", ".jpg", ".jpeg"]) . takeExtension

toggle :: MVar ThreadState -> IO ()
toggle v = modifyMVar_ v $ \s@(ThreadState{..}) -> do
    killThread threadId
    let inPomo = isJust pomo
    shuffled <- shuffle' files (length files) <$> newStdGen
    let newJob =
            if inPomo
                then mapM_ display shuffled
                else mapM_ (uncurry (displayPomo v)) (align shuffled)
    newThread <- forkIO newJob
    when inPomo $ notify "leaving pomo"
    -- pomo will be overwritten in displayPomo
    pure $ s{pomo = Nothing, threadId = newThread}
  where
    align = (`zip` times) . cycle

timeRemaining :: MVar ThreadState -> IO (Maybe NominalDiffTime)
timeRemaining v = do
    p <- pomo <$> readMVar v
    case p of
        Nothing -> pure Nothing
        Just p' -> do
            let (started, currState) = (startTime &&& state) p'
            diff <- (`diffUTCTime` started) <$> getCurrentTime
            pure . Just $ stateTime currState - diff

queryTime :: MVar ThreadState -> IO ()
queryTime v = (notify . T.pack . maybe "not in pomo" showTime) =<< timeRemaining v
  where
    showTime = render . (`divMod` 60) . round
    render (m, s) = show m ++ "m" ++ show s ++ "s"

setupDBus :: ThreadState -> IO ()
setupDBus state = do
    client <- connectSession
    -- the options we have here ensure that we don't need to check the result
    -- of the name request, but print result anyway to verify while testing
    requestName
        client
        (busName_ "org.jbrown.hallpaper")
        [nameAllowReplacement, nameReplaceExisting, nameDoNotQueue]
        >>= print
    stateVar <- newMVar state
    export
        client
        "/"
        defaultInterface
            { interfaceName = "org.jbrown.toggle"
            , interfaceMethods =
                [ autoMethod "toggle" (toggle stateVar)
                , autoMethod "time" (queryTime stateVar)
                ]
            }

start :: [FilePath] -> IO ThreadState
start files = do
    gen <- newStdGen
    let shuffled = shuffle' files (length files) gen
    threadId <- forkIO (mapM_ display shuffled)
    pure ThreadState{threadId = threadId, files = files, pomo = Nothing, gen = gen}

daemonMain :: IO ()
daemonMain = do
    Notify.init (Just "daemon")
    homePath <- home
    let fstream = ls (homePath ++ "/.config/sway/wallpapers")
    fs <- filter isImg <$> fold fstream Fold.list
    state <- start fs
    setupDBus state
    -- somewhat hacky way to make the main thread block indefinitely.
    -- it's possible that i should be thinking about gracefully exiting on error,
    -- via calling `disconnect`.
    newEmptyMVar >>= takeMVar

clientMain :: IO ()
clientMain = do
    client <- connectSession
    let req =
            (methodCall "/" "org.jbrown.toggle" "time")
                { methodCallDestination = Just "org.jbrown.hallpaper"
                }

    reply <- call client req

    -- Handle the reply
    case reply of
        Left error -> putStrLn $ "Error: " ++ show error
        Right result -> putStrLn $ "Received reply: " ++ show result
    disconnect client
