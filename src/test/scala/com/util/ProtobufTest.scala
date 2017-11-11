package com.util


import com.example.tutorial.addressbook.Person
import com.trueaccord.scalapb.json.JsonFormat
import org.json4s.JsonAST.JValue



// please, note - we are importing the mutable Specification and not normal,
// as the normal Specification requires "def is"
import org.specs2.mutable._


class ProtobufTest
  extends Specification {

  "round trip" in  {
    val person = Person(name = Some("Peter"), id = Some(1218))
    val r: String = JsonFormat.toJsonString(person)
    val j: JValue = JsonFormat.toJson(person)
    val p: Person = JsonFormat.fromJson[Person](j)

    ok
  }

}
