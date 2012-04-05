
name := "spark-admm"

organization := "edu.berkeley"

version := "1.0"

scalaVersion := "2.9.1"

resolvers ++= Seq(
            "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies  ++= Seq(
	    "org.spark-project" %% "spark-core" % "0.4-SNAPSHOT",
	    "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)