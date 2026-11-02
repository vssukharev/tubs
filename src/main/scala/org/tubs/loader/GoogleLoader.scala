
package org.tubs.loader

import scala.util.{Using, Try, Success, Failure}
import upickle.default.*
import scala.io.Source
import java.io.{BufferedWriter, FileWriter, InputStreamReader, FileInputStream, File, FileNotFoundException}
import java.util.Collections
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp;
import com.google.api.client.auth.oauth2.Credential
import com.google.api.services.drive.{DriveScopes, Drive}
import scala.jdk.CollectionConverters.*
import com.google.api.services.drive.model.{File as GFile}
import com.google.api.client.http.ByteArrayContent
import java.nio.charset.StandardCharsets
import scalafx.Includes.*
import scalafx.scene.control.*
import scalafx.scene.layout.{GridPane, HBox, VBox, Priority}
import scalafx.geometry.{Insets, Pos}

import org.tubs.table.Table

object GoogleLoader:
  val dataStore    = ".authdata"
  val secretsPath  = "src/main/resources/client_secrets.json"
  val appData      = "appDataFolder"
  val defaultFile  = "tubs-table.json"
  val root         = Collections.singletonList(appData)
  val jsonFactory  = GsonFactory.getDefaultInstance()
  val scopes       = Collections.singletonList(DriveScopes.DRIVE_APPDATA)
  val transport    = NetHttpTransport()
  val service      = getService()

  /**
   * Chooses a path in google drive. Returns empty
   * string if no file chosen.
   */
  def choose(): String = 
    val files = getListing()

    val inputField = new TextField():
      text = defaultFile
      hgrow = Priority.Always

    val fileGrid = new GridPane:
      hgap = 8
      vgap = 6
      padding = Insets(6)

    if files.isEmpty then
      val emptyLabel = new Label("(No files found)"):
        style = "-fx-text-fill: #6c757d; -fx-font-style: italic;"
      fileGrid.add(emptyLabel, 0, 0)
    else
      val numCols = 3
      files.zipWithIndex.foreach { (f, idx) =>
        val name = f.getName
        val btn = new Button(name):
          maxWidth = Double.MaxValue
          style = "-fx-alignment: CENTER-LEFT; -fx-font-size: 11px;"
          onAction = _ => inputField.text = name
        fileGrid.add(btn, idx % numCols, idx / numCols)
      }

    val gridScroll = new ScrollPane:
      content = fileGrid
      fitToWidth = true
      prefHeight = 120
      maxHeight = 200

    val inputSection = new HBox:
      spacing = 10
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("File:"),
        inputField
      )

    val contentBox = new VBox:
      spacing = 10
      padding = Insets(10)
      children = Seq(
        gridScroll,
        inputSection
      )

    val dialog = new Dialog[String]:
      title = "tubs"
      headerText = "Google Drive file chooser"
      dialogPane = new DialogPane:
        content = contentBox
        buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
      resultConverter = dialogButton =>
        if dialogButton == ButtonType.OK then inputField.text.value
        else null

    dialog.showAndWait() match
      case Some(res) if res != null && res.toString.trim.nonEmpty => res.toString.trim
      case _                                                      => throw new FileNotFoundException("Please enter some file")

  /**
   * Returns a resulting serialized Table.
   */
  def load(path: String): String = 
    val files = getListing()
    files.find(_.getName == path) match
      case Some(f) => 
        val is = service.files()
          .get(f.getId)
          .executeMediaAsInputStream()
        Source.fromInputStream(is).mkString
      case None => 
        throw new FileNotFoundException(s"Unknown file on Google Drive: $path")

  /**
   * Returns Google Drive file id of selected file.
   */
  def save(content: String, path: String): Unit = 
    val files = getListing()
    val metadata = GFile()
      .setName(path)
      .setMimeType("application/json")
    val mediaContent = ByteArrayContent(
      "application/json", 
      content.getBytes(StandardCharsets.UTF_8)
    )

    files.find(f => f.getName == path) match
      case Some(f) =>
        service.files()
          .update(f.getId, metadata, mediaContent)
          .execute()
      case None =>
        service.files()
          .create(metadata.setParents(root), mediaContent)
          .execute() 

  /**
   * Retrieves the list of files in format
   */
  def getListing(): List[GFile] =
    val fileList = service.files().list()
      .setSpaces(appData)
      .setFields("files(id, name)")
      .execute()
    val files = fileList.getFiles
    if files == null then Nil 
    else files.asScala.toList
    

  /**
   * Performs authorization with receiver under loopback port 6969.
   */
  def getCredential(): Credential =
    val is = InputStreamReader(FileInputStream(secretsPath))
    val secrets = GoogleClientSecrets.load(jsonFactory, is)
    val flow = GoogleAuthorizationCodeFlow.Builder(transport, jsonFactory, secrets, scopes)
      .setDataStoreFactory(FileDataStoreFactory(File(dataStore)))
      .build()
    val receiver = LocalServerReceiver.Builder()
      .setPort(6969)
      .build()
    AuthorizationCodeInstalledApp(flow, receiver)
      .authorize("user")

  /**
   * Retrieves drive service, automatically getting credentials
   * beforehand.
   */
  def getService(): Drive =
    Drive.Builder(transport, jsonFactory, getCredential())
      .setApplicationName("tubs")
      .build()

