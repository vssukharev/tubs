name := "tubs"
version := "0.1"
scalaVersion := "3.9.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "com.lihaoyi" %% "upickle" % "4.4.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

Compile / console / initialCommands := """
  import org.tubs.{_, given}
  import scala.language.implicitConversions
  import org.tubs.executor._
  import org.tubs.loader._
  import org.tubs.table._
  import _root_.upickle.default._
  val t = Table()
  given Table = t
"""

