
package org.tubs.loader

import java.io.{BufferedWriter, FileWriter, File}
import scala.util.{Using, Try, Success, Failure}
import upickle.default.*
import scala.io.Source

import org.tubs.utils.Todo
import org.tubs.table.Table

// TODO: get rid of Source
def load(path: String): Try[Table] = Using(Source.fromFile(path))((src: Source) => read[Table](src.mkString))
def save(path: String, t: Table): Try[Unit] = Using(FileWriter(path))(_.write(write(t)))

