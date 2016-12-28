package com.books
package fp

final case class Box(height: Double, width: Double)


object Box {

  def wider(x: Box, y: Box): Box = {
    greaterBy(x, y, _.width)
  }


  def taller(x: Box, y: Box): Box = {
    greaterBy(x, y, _.height)
  }


  private def greaterBy(x: Box, y: Box, f: Box => Double): Box = {
    if (f(x) > f(y)) x else y
  }

}
