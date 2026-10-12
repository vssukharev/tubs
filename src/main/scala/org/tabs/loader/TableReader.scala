
package org.tubs.loader

import java.io.{BufferedReader, FileReader}
import scala.util.{Using, Try, Success, Failure}
import org.tubs.table.*

def parseLine(line: String): (CellKey, Cell) = 
  val (rs, tail) = line.span(_ != ':')
  val (ls, content) = tail.drop(1).span(_ != ' ')
  val key = CellKey(rs.toInt, ls.toInt)
  (key, Cell(key, content.drop(1)))

class TableReader(name: String):
  def read(): Try[Table] = 
    var res = Table()
    val wrap = Using(BufferedReader(FileReader(name))) {
      _.lines()
       .filter(!_.isEmpty)
       .map(parseLine)
       .forEach(res.cells += _)
    }
    wrap match
      case Success(_) => Success(res)
      case Failure(e) => Failure(e)


