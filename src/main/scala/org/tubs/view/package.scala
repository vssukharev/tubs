
package org.tubs.view

import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color

def info(message: String): Unit  = println("INFO: " + message)
def warn(message: String): Unit  = println("WARNING: " + message)
def error(message: String): Unit = println("ERROR: " + message)

def mainScene = new Scene(900, 600):
  root = new StackPane:
    children = Seq(
      new BorderPane:

        // --- Top (Sections 1-4)
        top = new VBox:
          spacing = 5
          padding = Insets(5)
          
          val secFileName = new HBox:
            val label = new Label("File name"):
              style = "-fx-font-size: 16px; -fx-font-weight: bold;"
            alignment = Pos.Center
            children = Seq(label)

          val secFileManip = new ToolBar:
            items = Seq(
              Button("Open"),
              Button("Save"),
              Button("Save as (locally)"),
              Button("Save as (remotely)"),
              new Region { hgrow = Priority.Always },
              Button("Help")
            )

          val secStyleEditor = new ToolBar:
            val fgPicker = ColorPicker(Color.Black)
            val bgPicker = ColorPicker(Color.White)

            items = Seq(
              Label("Foreground color"),
              fgPicker,
              Label("Background color"),
              bgPicker
            )

          val secCurSelection = new HBox:
            spacing = 5

            val selectionIndex = new TextField:
              text = "A1"
              prefWidth = 60
              alignment = Pos.Center
              editable = false

            val selectionContent = new TextArea:
              text = "a"
              prefRowCount = 1
              hgrow = Priority.Always

            children = Seq(selectionIndex, selectionContent)

          children = Seq(secFileName, secFileManip, secStyleEditor, secCurSelection)

        // --- Central (Section 5)
        center = new TableView[String]

        // --- Bottom (Section 6)
        bottom = new HBox:
          padding = Insets(5)
          alignment = Pos.CenterRight
          spacing = 10
          style = "-fx-border-color: lightgray; -fx-border-width: 1 0 0 0;"
          
          children = Seq(
            Button("-"),
            Label("Scale 100%"),
            Button("+")
          )
    )
