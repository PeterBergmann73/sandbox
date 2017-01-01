name := "sandbox"

version := "1.0"

// we have to keep the version to be 2.11 to allow compilation for Spark
scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

def SpecsModule(name: String) = {
  "org.specs2" %% name % "3.6.5" % "test"
}

libraryDependencies += SpecsModule("specs2-core")

libraryDependencies += SpecsModule("specs2-matcher-extra")

libraryDependencies += SpecsModule("specs2-scalacheck")

libraryDependencies += SpecsModule("specs2-junit")

scalacOptions in Test ++= Seq("-Yrangepos")
