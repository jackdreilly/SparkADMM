import sbt._
import Keys._

object ADMMBuild extends Build {

    lazy val root = Project(id = "spark-admm",
                            base = file("."),
                            settings = Project.defaultSettings)
}