package com.books
package fp
package chapter6


sealed trait Input


case object Coin extends Input


case object Turn extends Input


// p. 86. The rules of the machine are as follows:
// - Inserting a coin into a locked machine will cause it to unlock if there is
// any candy left.
// - Turning the knob on an unlocked machine will cause it to dispense candy
// and become locked.
// - Turning the knob on a locked machine or inserting a coin into an
// unlocked machine does nothing.
// - A machine that is out of candy ignores all inputs.
final case class Machine(locked: Boolean, candies: Int, coins: Int) {

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
  }


  def update(i: Input)(s: Machine): Machine = {
    (i, s) match {
      case (_, Machine(_, 0, _))               => s
      case (Coin, Machine(false, _, _))        => s
      case (Turn, Machine(true, _, _))         => s
      case (Coin, Machine(true, candy, coin))  => s.copy(locked = false, coins = coin + 1)
      case (Turn, Machine(false, candy, coin)) => s.copy(locked = true, candies = candy - 1)
    }
  }

}