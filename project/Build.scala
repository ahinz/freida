import sbt._
import Keys._

object FreidaBuild extends Build {
  val vers = "0.8.7"
  val uvers = "0.6.1"

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

      "com.codahale" %% "jerkson" % "0.5.0",

      "com.mongodb.casbah" %% "casbah" % "2.1.5-1",

      "net.databinder" %% "unfiltered-filter" % uvers,
      "net.databinder" %% "unfiltered-jetty" % uvers,
      "org.clapper" %% "avsl" % "0.3.6"
    ),
    resolvers += "repo.codahale.com" at "http://repo.codahale.com"
//    scalacOptions ++= Seq("-unchecked", "-deprecation")
  )


}
