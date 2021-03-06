name := "MacLogic2"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

assemblyJarName in assembly := "MacLogic2.jar"
assemblyOutputPath in assembly := file("out/MacLogic2.jar")
mainClass in assembly := Some("MacLogic2")
