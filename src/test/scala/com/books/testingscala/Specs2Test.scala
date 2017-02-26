package com.books.testingscala

import com.books.testingscala.chapter1.Album
import com.util.Specs2Spec
import org.specs2.matcher.XmlMatchers


/** We need XmlMatchers for the xml test below. */
class Specs2Test extends Specs2Spec with XmlMatchers {

  "ok" in {
    ok
  }

  "must" in {
    3 must_== 3
  }

  "exception" in {
    Album("brr", 1910, None) must throwAn("No Artist provided")
  }

  "skipped" in {
    skipped("skipped for the time being")
    3 must_== 3
  }

  "close matcher" in {
    (0.9 - 0.8) must beCloseTo(0.1, 0.01)
  }

  "iterable matchers" in {
    val l = List(1, 2, 3)
    l must not be empty
    l must contain(3)
    l must not contain 5

    // it does not work
    // List("Hello", "World") must have(_.size >= 5)
  }

  "map matchers" in {
    val map = Map("Jimmy Page" -> "Led Zeppelin", "Sting" -> "The Police", "Aimee Mann" -> "Til\' Tuesday")
    map must haveKey("Sting")
    map must haveValue("Led Zeppelin")
    map must not haveKey "Brian May"
    map must havePair("Aimee Mann" -> "Til\' Tuesday")
  }

  "xml matchers" in {
    val coldPlayAlbums = <albums>
      <album name="Parachutes"/>
      <album name="A Rush of Blood to the Head"/>
      <album name="X&amp;Y"/>
      <album name="Viva la Vida or Death and All His Friends"/>
      <album name="Mylo Xyloto"/>
    </albums>

    // methor beEqualToIgnoringSpace is from XmlMatchers
    coldPlayAlbums must beEqualToIgnoringSpace(<albums>
      <album name="Parachutes"/>
      <album name="A Rush of Blood to the Head"/>
      <album name="X&amp;Y"/>
      <album name="Viva la Vida or Death and All His Friends"/>
      <album name="Mylo Xyloto"/>
    </albums>)
  }

  "partial function matchers" in {
    val goldPartialFunction: PartialFunction[Int, String] = new PartialFunction[Int, String] {
      //States that this partial function will take on the task
      def isDefinedAt(x: Int): Boolean = x >= 500000

      //What we do if this does partial function matches
      def apply(v1: Int) = "Gold"
    }

    val platinumPartialFunction: PartialFunction[Int, String] = {
      case x: Int if x >= 1000000 => "Platinum"
    }

    val junkPartialFunction: PartialFunction[Int, String] = {
      case x: Int if x < 500000 => "Alternative"
    }

    val riaaCertification: PartialFunction[Int, String] =
      goldPartialFunction orElse platinumPartialFunction orElse junkPartialFunction

    riaaCertification must beDefinedAt(100)
    riaaCertification must beDefinedBy(100 -> "Alternative")
  }

}
