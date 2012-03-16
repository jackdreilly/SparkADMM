
name := "spark-admm"

organization := "edu.berkeley"

version := "1.0"

scalaVersion := "2.9.1"


libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT"
)

resolvers ++= Seq(
            // other resolvers here
            "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

libraryDependencies+= "org.spark-project" % "spark-core_2.9.1" % "0.4-SNAPSHOT"