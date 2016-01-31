package com.chess.board

import com.util.CollectionUtil


abstract class Coordinate {

  implicit def toInt: Int

  def move(i: Int): Option[Coordinate] = {
    fromBoardIndex(this.boardIndex + i)
  }

  /**
    * the field is not declared as protected to allow tests to access it.
    * the field should not be used outside of the tests.
    */
  def all: Seq[Coordinate]

  protected def boardIndex: Int = {
    all.zipWithIndex.map {
      case (c, i) if c == this => i
    }.head
  }

  protected def fromBoardIndex(int: Int): Option[Coordinate] = {
    CollectionUtil.safeToMap(all.zipWithIndex.map {
      case (p, i) => (i, p)
    }).get(int)
  }

}
