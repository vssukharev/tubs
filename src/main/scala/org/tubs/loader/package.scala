
package org.tubs.loader

import java.io.{BufferedWriter, FileWriter, File}
import scala.util.{Using, Try, Success, Failure}
import upickle.default.*
import scala.io.Source

import org.tubs.utils.Todo
import org.tubs.table.Table

def load(path: String): Try[Table] = 
  Using( Source.fromFile(path) ) {
    src => read[Table](src.mkString)
  }

def save(t: Table, path: String): Try[Unit] = 
  Using( FileWriter(path) ) {
    wter => wter.write(write(t, 2))
  }

