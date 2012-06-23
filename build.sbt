name := "erlang-pattern-parser"

organization := "com.github.mdr"

version := "0.0.1-SNAPSHOT"

libraryDependencies += "org.erlang.otp" % "jinterface" % "1.5.6"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0", "2.9.1", "2.9.2")

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"
 
EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

