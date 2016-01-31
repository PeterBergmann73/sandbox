package com.chess
package piece

import com.chess.board.Cell


abstract class Piece {

  def colour: Colour

  def nextsToEmpty(cell: Cell): Seq[Cell]

  def nextsToOccupied(cell: Cell): Seq[Cell]
}
