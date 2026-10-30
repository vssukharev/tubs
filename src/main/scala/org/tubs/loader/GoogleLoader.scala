
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
import scalafx.scene.control.*

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
    val dialog = new TextInputDialog(defaultFile):
      title = "tubs"
      headerText = "Google Drive file chooser"
      contentText = "Enter file to be chosen:"
    dialog.showAndWait() match
      case Some(res) => res
      case None      => throw new FileNotFoundException("Please select enter file")

  /**
   * Returns a resulting serialized Table.
   */
  def load(path: String): String = 
    val result = service.files().list()
      .setSpaces(appData)
      .setFields("files(id, name)")
      .execute()

    result.getFiles.asScala.find(_.getName() == path) match
      case Some(f) => 
        val id = f.getId()
        val is = service.files()
          .get(id)
          .executeMediaAsInputStream()
        Source.fromInputStream(is).mkString
      case None => 
        val res = write(Table(), 2)
        save(res, path)
        res

  /**
   * Returns Google Drive file id of selected file.
   */
  def save(content: String, path: String): Unit = 
    val metadata = GFile()
      .setName(path)
      .setMimeType("text/plain")
      .setParents(root)
    val mediaContent = ByteArrayContent("text/plain", content.getBytes(StandardCharsets.UTF_8))
    service.files()
      .create(metadata, mediaContent)
      .execute() 

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

