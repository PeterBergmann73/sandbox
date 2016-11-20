package com.chess

import com.BaseSpec
import com.chess.board.Vertical


class VerticalTest extends BaseSpec {

  "toString" should {

    "print letter" in {
      val a = Vertical.a
      a.toString shouldEqual "a"
    }

  }

}
