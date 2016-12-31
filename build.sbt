name := "sandbox"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// https://mvnrepository.com/artifact/org.specs2/specs2-core_2.12
// libraryDependencies += "org.specs2" % "specs2-core_2.12" % "3.8.6" % "test"

def SpecsModule(name: String) = {
  "org.specs2" %% name % "3.6.5" % "test"
}

libraryDependencies += SpecsModule("specs2-core")

libraryDependencies += SpecsModule("specs2-matcher-extra")

libraryDependencies += SpecsModule("specs2-scalacheck")

libraryDependencies += SpecsModule("specs2-junit")

//libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.5" % "test")

// scalacOptions in Test ++= Seq("-Yrangepos")
