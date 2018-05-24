name := "fpinscala"

version := "1.0"

scalaVersion in ThisBuild := "2.12.3"

scalacOptions ++= Seq(
    "-deprecation",  // issue warning if we use any deprecated API features
    "-feature",      // tells the compiler to provide information about misused language features
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Xlint",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code"
  )
