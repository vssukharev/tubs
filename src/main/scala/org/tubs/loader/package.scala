
package org.tubs.loader

import java.io.{BufferedWriter, FileWriter, File}
import scala.util.{Using, Try, Success, Failure}
import upickle.default.*
import scala.io.Source
import scalafx.stage.FileChooser

import org.tubs.utils.Todo
import org.tubs.table.Table

enum LocationKind:
  case Local
  case Remote
  override def toString: String = this match
    case Local => "locally"
    case Remote => "remotely"

object Loader:  

  /**
   * Loads a table from path with specified location kind.
   */
  def load(kind: LocationKind, path: String = ""): Try[(Table, String)] =
    kind match
      case LocationKind.Local  => loadLocal(path)
      case LocationKind.Remote => loadRemote(path)

  /**
   * Saves a table into path with specified location kind and returns
   * a file name of a resulting file. 
   *
   * If path is retained empty, user is prompted to select an eligible
   * file (either locally or remotely).
   */
  def save(t: Table, kind: LocationKind, path: String = ""): Try[String] =
    kind match
      case LocationKind.Local  => saveLocal(t, path)
      case LocationKind.Remote => saveRemote(t, path)

  def loadLocal(path: String = ""): Try[(Table, String)] = 
    val resPath = if path != "" then path else
      val chooser = new FileChooser:
        title = "Open Table File"
      val file = chooser.showOpenDialog(null)
      if file != null then file.getAbsolutePath else ""

    Using(Source.fromFile(resPath)) { src => 
      val res = read[Table](src.mkString)
      (res -> resPath)
    }

  def saveLocal(t: Table, path: String = ""): Try[String] = 
    val resPath = if path != "" then path else
      val chooser = new FileChooser:
        title = "Save Table As"
      val file = chooser.showSaveDialog(null)
      if file != null then file.getAbsolutePath else ""

    Using(FileWriter(resPath)) { wter => 
      wter.write(write(t, 2))
      resPath
    }

  def loadRemote(path: String = ""): Try[(Table, String)] =
    try
      val resPath = if path == "" then GoogleLoader.choose() else path
      val s = GoogleLoader.load(resPath)
      val res = read[Table](s)
      Success(res -> resPath)
    catch
      case e => Failure(e)
    

  def saveRemote(t: Table, path: String = ""): Try[String] =
    try
      val resPath = if path == "" then GoogleLoader.choose() else path
      GoogleLoader.save(write(t, 2), resPath)
      Success(resPath)
    catch
      case e => Failure(e)

