module Main where

import qualified Control.Foldl as Fold
import System.FilePath (takeExtension)
import System.Random
import System.Random.Shuffle
import Turtle

times :: [NominalDiffTime]
times = map (* 60) $ cycle [20, 5, 20, 5, 20, 5, 20, 15]

display :: FilePath -> NominalDiffTime -> IO ()
display file time =
    procs
        "swww"
        ["img", format fp file, "--transition-fps", "140", "--transition-type", "center"]
        empty
        >> sleep time

isImg :: FilePath -> Bool
isImg = (`elem` [".gif", ".png", ".jpg", ".jpeg"]) . takeExtension

main :: IO ()
main = do
    homePath <- home
    let fstream = ls (homePath ++ "/.config/sway/wallpapers")
    fs <- filter isImg <$> fold fstream Fold.list
    shuffled <- shuffle' fs (length fs) <$> newStdGen
    mapM_ (uncurry display) (align shuffled)
  where
    align = (`zip` times) . cycle
