package com.str

sealed abstract class Strat {

  def apply(ind: Int): Double

  def +(other: Strat): Strat = Strat.Sum(this, other)

  def -(other: Strat): Strat = Strat.Minus(this, other)

  def *(other: Strat): Strat = Strat.Product(this, other)

  def /(other: Strat): Strat = Strat.Ratio(this, other)

}


trait WithInVal {

  def inVal: Double

}


object Strat {

  val Tiny = 1.0e-13

  final case class Identity(d: Double) extends Strat {
    def apply(ind: Int): Double = d
  }

  implicit def fromDouble(d: Double): Strat = Identity(d)

  final case class Sum(one: Strat, another: Strat) extends Strat {
    def apply(ind: Int): Double = one(ind) + another(ind)
  }

  final case class Minus(one: Strat, another: Strat) extends Strat {
    def apply(ind: Int): Double = one(ind) - another(ind)
  }

  final case class Product(one: Strat, another: Strat) extends Strat {
    def apply(ind: Int): Double = one(ind) * another(ind)
  }

  final case class Ratio(one: Strat, another: Strat) extends Strat {
    def apply(ind: Int): Double = {
      val anotherVal = another(ind)
      require(math.abs(anotherVal) > Tiny, "Oops")
      one(ind) / another(ind)
    }
  }

  final case class Ln(inner: Strat) extends Strat {
    def apply(ind: Int): Double = {
      val innerVal = inner(ind)
      require(innerVal > 0.0, "Oops")
      math.log(innerVal)
    }
  }

  final case class Exp(inner: Strat) extends Strat {
    def apply(ind: Int): Double = {
      math.exp(inner(ind))
    }
  }

  final case class Grow(inner: Int => Double, inVal: Double) extends Strat with WithInVal {

    def apply(ind: Int): Double = {
      ind match {
        case 0 => inVal
        case i =>
          if (i < 0) sys.error("Oops")
          else apply(i - 1) * (1.0 + inner(i - 1))
      }
    }
  }

  final case class Ret(inner: Int => Double) extends Strat {

    def apply(ind: Int): Double = {
      if (ind <= 0) sys.error("Oops")
      else {
        val prev = inner(ind - 1)
        require(math.abs(prev) > Tiny, "Oops")
        inner(ind) / prev
      }
    }

  }

  final case class Max(inner1: Strat, inner2: Strat) extends Strat {

    def apply(ind: Int): Double = math.max(inner1(ind), inner2(ind))

  }

  final case class Min(inner1: Strat, inner2: Strat) extends Strat {

    def apply(ind: Int): Double = math.min(inner1(ind), inner2(ind))

  }

}


