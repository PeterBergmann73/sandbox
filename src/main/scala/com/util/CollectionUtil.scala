package com.util


object CollectionUtil {

  def safeToMap[A, B](seq: Seq[(A, B)]): Map[A, B] = {
    seq.groupBy(_._1).map {
      case (k, v) =>
        val v0 = v match {
          case Seq(only) => only._2
          case e => sys.error(s"More than 1 value $e found for $k")
        }

        (k, v0)
    }
  }


  def swap[A, B](seq: Seq[(A, B)]): Seq[(B, A)] = {
    seq.map {
      case (v, k) => (k, v)
    }
  }

}
