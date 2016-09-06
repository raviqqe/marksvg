module Main where

import CMark as M
import Control.Monad
import Control.Applicative
import Data.Text
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg.Renderer.String


main :: IO ()
main = do
  n <- readMarkdown

  putStrLn $ show (n :: Node)

  case markdownToSvg n of
    Left e -> putStrLn $ e
    Right m -> putStrLn $ renderSvg m


readMarkdown :: IO M.Node
readMarkdown = M.commonmarkToNode [] <$> pack <$> getContents


markdownToSvg :: Node -> Either String S.Markup
markdownToSvg n = do
  Left "No"
