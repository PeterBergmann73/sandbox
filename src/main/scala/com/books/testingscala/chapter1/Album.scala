package com.books
package testingscala
package chapter1

import com.books.testingscala.chapter3.Artist

final case class Album(title: String, year: Int, artist: Artist)


object Album {

  def apply(title: String,
            year: Int,
            artist: Option[Artist]): Album = {

    artist match {
      case None    =>
        sys.error("No Artist provided")
      case Some(a) =>
        Album(title, year, a)
    }
  }

}

