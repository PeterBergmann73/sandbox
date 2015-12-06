package com.str


abstract class StratManager {

}


object StratManager1 extends StratManager {

  val seq1 = Seq(0.1, 0.2, 0.3, 0.4, 0.5)
  val seq2 = Seq(0.6, 0.7, 0.8, 0.9, 1.0)
  val grower1 = Strat.Grow(seq1, 3.0)
  val grower2 = Strat.Grow(seq2, 6.0)

  val mixer = Strat.Min(4.0 * grower1, 3.2 * grower2)

  val res = seq1.indices.map(i => mixer)

  println(s"res: $res")
}
