
package org.tubs.view

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.effect.DropShadow
import scalafx.scene.transform.Scale
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.stage.FileChooser
import scalafx.beans.property.{ObjectProperty, StringProperty, IntegerProperty, DoubleProperty}

object Sections:
  // 1. File name (Fixed Box)
  val fileNameLabel = new Label("File name"):
    text <== View.currentFileName
    style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-text-fill: #2c3e50;"

  val fileName = new HBox:
    alignment = Pos.Center
    padding = Insets(5, 15, 5, 15)
    style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 1px; -fx-background-radius: 4px; -fx-border-radius: 4px;"
    children = Seq(fileNameLabel)

  // 2. Action buttons + Help
  val fileManip = new ToolBar:
    items = Seq(
      Buttons.open,
      Buttons.save,
      Buttons.saveAsLocally,
      Buttons.saveAsRemotely,
      new Region { hgrow = Priority.Always },
      Buttons.help
    )

  // 3. Style editor (Foreground and background color pickers)
  val fgPicker = new ColorPicker(Defaults.DefaultFgColor):
    style = "-fx-color-label-visible: false;"
    prefWidth = 45

  val bgPicker = new ColorPicker(Defaults.DefaultBgColor):
    style = "-fx-color-label-visible: false;"
    prefWidth = 45

  val styleEditor = new ToolBar:
    items = Seq(
      new Label(" Foreground color: "),
      fgPicker,
      new Separator(),
      new Label(" Background color: "),
      bgPicker
    )

  // 4. Selection pill & expandable multiline formula input
  val selectionIndex = new Label("A1"):
    alignment = Pos.Center
    minWidth = 65
    prefHeight = 28
    style = s"-fx-background-color: ${Defaults.SelectionPillBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-radius: 14px; -fx-background-radius: 14px; -fx-font-weight: bold; -fx-text-fill: #495057; -fx-font-size: 12px;"

  val selectionContent = new TextArea:
    promptText = "Formula or text (e.g. =A1 + B2)"
    prefRowCount = 1
    maxHeight = 28
    prefHeight = 28
    hgrow = Priority.Always
    wrapText = true
    style = "-fx-font-family: 'monospace'; -fx-font-size: 13px;"

  val formulaInputContainer = new StackPane:
    alignment = Pos.TopLeft
    hgrow = Priority.Always
    prefHeight = 28
    children = Seq(selectionContent)

  val curSelection = new HBox:
    spacing = 8
    alignment = Pos.TopLeft
    padding = Insets(2, 4, 4, 4)
    children = Seq(selectionIndex, formulaInputContainer)

  // 5. Scroll pane and inner grid sections
  val grid = new GridPane:
    style = s"-fx-background-color: ${Defaults.GridBgColor};"

  val scrollPane = new ScrollPane:
    content = grid
    fitToWidth = false
    fitToHeight = false
    hgrow = Priority.Always
    vgrow = Priority.Always
  

