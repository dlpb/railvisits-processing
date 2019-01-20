name := """stations-scala"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.4.0"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.4.0"
libraryDependencies += "org.scalaj" % "scalaj-http_2.11" % "2.3.0"

fork in run := true