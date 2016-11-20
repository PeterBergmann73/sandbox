package com.chess.board

import com.chess.Colour

final case class Cell(vertical: Vertical, horizontal: Horizontal) {

  def colour: Colour = {
    (vertical.toInt + horizontal.toInt) % 2 match {
      case 0 => Colour.Black
      case _ => Colour.White
    }
  }

  override def toString = s"${vertical.toString}${horizontal.toString}"

}
