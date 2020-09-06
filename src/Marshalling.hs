{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Marshalling where

import Handler.Game
import Import
--import Game.Board (Cell(Cell))

--fromCellEntry :: Game -> Cell
--fromCellEntry isFlagged isRevealed hasBomb neighboringBombs = Cell {
--isFlagged = isFlagged, isRevealed = isRevealed, hasBomb = hasBomb, neighboringBombs = neighboringBombs
--}