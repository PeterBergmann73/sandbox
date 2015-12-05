package com.demo

import com.BaseSpec

/**
  * Created by Slava on 29/11/2015.
  */
class HelloWorldTest extends BaseSpec {

  val hello = new HelloWorld

  "test" should {
    "produce correct result" in {
      hello.sayHello("scala") should equal("Hello, scala")
    }
  }

}
