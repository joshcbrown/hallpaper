module Main where

import qualified Control.Foldl as Fold
import System.FilePath (takeExtension)
import Turtle

display :: FilePath -> IO ()
display file = procs "swww" ["img", format fp file] empty >> sleep 5

isImg :: FilePath -> Bool
isImg = (`elem` [".gif", ".png", ".jpg", ".jpeg"]) . takeExtension

main :: IO ()
main = do
    homePath <- home
    let fstream = ls (homePath ++ "/.config/sway/wallpapers")
    fs <- filter isImg <$> fold fstream Fold.list
    mapM_ display (cycle fs)
