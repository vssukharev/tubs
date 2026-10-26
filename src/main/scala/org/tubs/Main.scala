
package org.tubs

import scala.util.{Try, Success, Failure}
import scalafx.application.JFXApp3

import org.tubs.view.*
import org.tubs.table.Table

object Main extends JFXApp3:

  override def start(): Unit =
    val filename = "test.txt"

    println("Loading " + filename)
    val table = loader.load(filename) match
      case Success(v) => v
      case Failure(e) => 
        error(e.getMessage)
        Table()

    println("Resulting table:")
    println(table.toString)

    stage = new JFXApp3.PrimaryStage:
      title = "tubs - Spreadsheet Editor"
      resizable = true
      scene = View.createScene(table, filename)

