package com.chess.board

import com.util.CollectionUtil


sealed abstract class Vertical extends Coordinate {

  implicit def toInt: Int = {
    val ints: Seq[(Vertical, Int)] = all.zipWithIndex.map {
      case (c, i) => (c, i + 1)
    }

    val mapped: Map[Vertical, Int] = CollectionUtil.safeToMap(ints)
    mapped(this)
  }

  override def move(i: Int): Option[Vertical] =
    super.move(i).map(c => c.asInstanceOf[Vertical])

  implicit def fromString(s: String): Option[Vertical] = {
    val swapped: Seq[(String, Vertical)] = all.map(c => c.toString -> c)
    val mapped: Map[String, Vertical] = CollectionUtil.safeToMap(swapped)
    mapped.get(s)
  }

  /**
    * the field is not declared as protected to allow tests to access it.
    * the field should not be used outside of the tests.
    */
  val all: Seq[Vertical] = {
    import Vertical.{a, b, c, d, e, f, g, h}
    Seq(a, b, c, d, e, f, g, h)
  }

}

object Vertical {

  case object a extends Vertical

  case object b extends Vertical

  case object c extends Vertical

  case object d extends Vertical

  case object e extends Vertical

  case object f extends Vertical

  case object g extends Vertical

  case object h extends Vertical

}


