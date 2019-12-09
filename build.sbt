name := "pcode"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.8"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
scalacOptions += "-Ypartial-unification"