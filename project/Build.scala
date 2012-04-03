import sbt._
import Keys._

object FreidaBuild extends Build {
  val vers = "0.8.7"

  lazy val project = Project(
    id = "fredia",
    base = file("."),
    settings = Defaults.defaultSettings
  ) settings (
    libraryDependencies ++=  Seq(
      "net.databinder" %% "dispatch-core" % vers,
      "net.databinder" %% "dispatch-oauth" % vers,
      "net.databinder" %% "dispatch-nio" % vers,
      "net.databinder" %% "dispatch-http" % vers,
      "net.databinder" %% "dispatch-tagsoup" % vers,
      "net.databinder" %% "dispatch-jsoup" % vers,
      "com.mongodb.casbah" %% "casbah" % "2.1.5-1"
    )
  )


}
