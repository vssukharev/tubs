
package org.tubs.loader

import org.tubs.utils.Todo
import org.tubs.table.Table
import scala.util.Try

def load(name: String): Try[Table] = 
  val reader = TableReader(name)
  reader.read()

