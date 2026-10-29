
package org.tubs

import scala.util.{Try, Success, Failure}
import scalafx.application.JFXApp3

import org.tubs.view.*
import org.tubs.table.Table
import org.tubs.loader.{GoogleLoader, Loader}

object Main extends JFXApp3:

  override def start(): Unit =
    val args = parameters.raw
    val filename = if args.length == 0 then "Empty Table" else args(0)

    val table = if args.length == 0 then Table() else
      Loader.loadLocal(filename) match
        case Success(v -> _) => v
        case Failure(e) =>
          error(s"Could not open a file $filename. Reason: ${e.getMessage}")
          Table()

    stage = new JFXApp3.PrimaryStage:
      title = "tubs - Spreadsheet Editor"
      resizable = true
      scene = View.createScene(table, filename)


