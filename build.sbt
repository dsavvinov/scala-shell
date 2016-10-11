name := "ScalaShell"

version := "1.0"

scalaVersion := "2.11.8"

artifactName := { (v: ScalaVersion, m: ModuleID, a: Artifact) => a.name }

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test"