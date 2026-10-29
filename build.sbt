name := "tubs"
version := "0.1"
scalaVersion := "3.9.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "com.lihaoyi" %% "upickle" % "4.4.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.scalafx" %% "scalafx" % "27.0.0-R39",

  // Google API deps
  "com.google.api-client" % "google-api-client" % "2.2.0",
  "com.google.oauth-client" % "google-oauth-client-jetty" % "1.34.1",
  "com.google.apis" % "google-api-services-drive" % "v3-rev20230822-2.0.0",
)

Compile / console / initialCommands := """
  import org.tubs.{*, given}
  import scala.language.implicitConversions
  import org.tubs.executor._
  import org.tubs.loader._
  import org.tubs.table._
  import _root_.upickle.default._
  val t = Table()
  given Table = t
"""

