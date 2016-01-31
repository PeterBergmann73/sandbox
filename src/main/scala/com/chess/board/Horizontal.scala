package com.chess.board

import com.util.CollectionUtil

sealed abstract class Horizontal extends Coordinate {

  implicit def toInt: Int

  override def move(i: Int): Option[Horizontal] =
    super.move(i).map(c => c.asInstanceOf[Horizontal])

  override def toString: String = toInt.toString

  implicit def fromInt(i: Int): Option[Horizontal] = {
    val swapped: Seq[(Int, Horizontal)] = all.zipWithIndex.map {
      case (c, ind) => (ind + 1, c)
    }
    val mapped: Map[Int, Horizontal] = CollectionUtil.safeToMap(swapped)
    mapped.get(i)
  }

  /**
    * the field is not declared as protected to allow tests to access it.
    * the field should not be used outside of the tests.
    */
  val all: Seq[Horizontal] = {
    import Horizontal.{One, Two, Three, Four, Five, Six, Seven, Eight}
    Seq(One, Two, Three, Four, Five, Six, Seven, Eight)
  }
}


object Horizontal {

  case object One extends Horizontal {
    def toInt: Int = 1
  }

  case object Two extends Horizontal {
    def toInt: Int = 2
  }

  case object Three extends Horizontal {
    def toInt: Int = 3
  }

  case object Four extends Horizontal {
    def toInt: Int = 4
  }

  case object Five extends Horizontal {
    def toInt: Int = 5
  }

  case object Six extends Horizontal {
    def toInt: Int = 6
  }

  case object Seven extends Horizontal {
    def toInt: Int = 7
  }

  case object Eight extends Horizontal {
    def toInt: Int = 8
  }

}
