name := "sandbox"

version := "1.0"

// we have to keep the version to be 2.11 to allow compilation for Spark
scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

def SpecsModule(name: String) = {
  "org.specs2" %% name % "3.6.5" % "test"
}

libraryDependencies += SpecsModule("specs2-core")

libraryDependencies += SpecsModule("specs2-matcher-extra")

libraryDependencies += SpecsModule("specs2-scalacheck")

libraryDependencies += SpecsModule("specs2-junit")

scalacOptions in Test ++= Seq("-Yrangepos")

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)


libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"

libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.2"
