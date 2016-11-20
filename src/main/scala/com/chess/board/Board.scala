package com.chess.board

import com.chess.piece.Piece


class Board(initial: Map[Cell, Piece]) {

  def cells: Seq[Cell] = ???

  val current: scala.collection.mutable.Map[Cell, Piece] = {
    scala.collection.mutable.Map(initial.toSeq: _*)
  }

  def move(from: Cell, to: Cell) = {
    if (isPossible(from, to)) {
      sys.error(s"Illegal move from $from to $to")
    } else {
      val piece = current.getOrElse(from, sys.error(s"No piece found at $from"))
      current - from
      current - to
      current + (to -> piece)
    }

  }

  private def isPossible(from: Cell, to: Cell): Boolean = ???
}
