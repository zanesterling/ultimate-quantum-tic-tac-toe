{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board (
    MetaBoard(MetaBoard)
  , Board(Board)
  , Cell(Empty, Token)
  , newBoard
) where

import Data.List
import Data.List.Split


data MetaBoard = MetaBoard [Board]
data Board     = Board [Cell]
data Cell      = Empty | Token Char

boardRowBlank = "   |   |   "
boardRowSep   = "---|---|---"
metaBoardRowBlank = intercalate "|" $ replicate 3 $ replicate cellWidth ' '
  where cellWidth = length boardRowSep + 2
metaBoardRowSep = intercalate "|" $ replicate 3 $ replicate cellWidth '-'
  where cellWidth = length boardRowSep + 2

instance Show MetaBoard where
  show (MetaBoard m) = intercalate "\n" $ wrap metaBoardRowBlank $ intersperse metaBoardRowSep $ map joinRow rowCells
    where rowCells = chunksOf 3 $ map show m
          joinRow row = intercalate "\n" $ map (wrap ' ' . intercalate " | ") $ transpose $ map lines row

instance Show Board where
  show (Board b) = intercalate "\n" $ wrap boardRowBlank $ intersperse boardRowSep rows
    where rows     = map (wrap ' ' . intercalate " | ") rowCells
          rowCells = chunksOf 3 $ map show b

instance Show Cell where
  show (Token c) = [c]
  show Empty     = " "


newBoard :: MetaBoard
newBoard = MetaBoard $ replicate 9 $ Board $ replicate 9 Empty

wrap :: a -> [a] -> [a]
wrap a as = a:(as ++ [a])
