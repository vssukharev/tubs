
package org.tubs

import scala.util.{Try, Success, Failure}
import org.tubs.view.*
import org.tubs.table.Table


@main def main(filename: String) =
  info("Loading " + filename)
  val table = loader.load(filename) match
    case Success(v) => v
    case Failure(e) => 
      error(e.getMessage)
      Table()  
  println("Resulting table:")
  println(table.toString)

