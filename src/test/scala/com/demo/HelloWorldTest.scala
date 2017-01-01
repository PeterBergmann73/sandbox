package com.demo

import com.BaseSpec


class HelloWorldTest extends BaseSpec {

  val hello = new HelloWorld

  "test" should {
    "produce correct result" in {
      hello.sayHello("scala") should equal("Hello, scala")
    }
  }

  "test brr" should {
    "trigger test" in {
      1 should equal(1)
    }
  }

}
