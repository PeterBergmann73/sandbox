package com.books
package testingscala

import org.scalatest.{FunSpec, Matchers}


class AlbumTest extends FunSpec with Matchers {

  describe("An Album") {
    it("can add an Artist object to the album") {
      val album = new Album("Thriller", 1981,
        new Artist("Michael", "Jackson"))

      album.artist.firstName should be ("Michael")
    }
  }

}
