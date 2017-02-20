package com.books
package testingscala
package chapter1


import com.books.testingscala.chapter3.Artist
import org.scalatest.{FunSpec, Matchers}


class AlbumTest extends FunSpec with Matchers {

  describe("An Album") {
    it("can add an Artist object to the album") {
      val album = Album("Thriller", 1981, Artist("Michael", "Jackson"))

      album.artist.firstName should be ("Michael")
    }
  }

}
