ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

libraryDependencies += "io.circe" %% "circe-generic" % "0.14.5"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "PumlParserScala"
  )
