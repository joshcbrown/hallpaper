module Wallpapers where

import qualified Control.Foldl as Fold
import qualified GI.Notify as Notify
import System.FilePath (takeExtension)
import System.Random
import System.Random.Shuffle
import Turtle

data PomoState = Work | ShortBreak | LongBreak

times :: [PomoState]
times = cycle [Work, ShortBreak, Work, ShortBreak, Work, ShortBreak, Work, LongBreak]

stateTime :: PomoState -> NominalDiffTime
stateTime Work = 20 * 60
stateTime ShortBreak = 5 * 60
stateTime LongBreak = 15 * 60

notify :: PomoState -> IO ()
notify state =
    let message = case state of
            Work -> "work"
            ShortBreak -> "short break"
            LongBreak -> "long break"
     in Notify.notificationShow =<< Notify.notificationNew message Nothing Nothing

display :: FilePath -> PomoState -> IO ()
display file state =
    procs
        "swww"
        ["img", format fp file, "--transition-fps", "140", "--transition-type", "center"]
        empty
        >> notify state
        >> sleep (stateTime state)

isImg :: FilePath -> Bool
isImg = (`elem` [".gif", ".png", ".jpg", ".jpeg"]) . takeExtension

daemonMain :: IO ()
daemonMain = do
    homePath <- home
    let fstream = ls (homePath ++ "/.config/sway/wallpapers")
    fs <- filter isImg <$> fold fstream Fold.list
    shuffled <- shuffle' fs (length fs) <$> newStdGen

    Notify.init $ Just "wallpaperd"
    mapM_ (uncurry display) (align shuffled)
  where
    align = (`zip` times) . cycle
