ThisBuild / scalaVersion := "3.4.2"

ThisBuild / organization := "com.slex"
ThisBuild / name := "slex"
ThisBuild / version := "0.1.0"


lazy val root = (project in file("."))
  .settings(
    name := "slex",
    version := "0.1.0",
    scalaVersion := "3.3.2",
    assembly / mainClass := Some("com.slex.Main")
  )

assembly / assemblyJarName := "slex.jar"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0"