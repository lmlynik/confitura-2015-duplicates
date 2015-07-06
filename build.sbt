name := "confitura-2015-duplicates"

organization := "pl.mlynik"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.7"

// Java version
javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked", "utf8")

// testing
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"