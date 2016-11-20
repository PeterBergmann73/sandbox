package com.chess

sealed abstract class Colour

object Colour {

  case object White extends Colour

  case object Black extends Colour

}
