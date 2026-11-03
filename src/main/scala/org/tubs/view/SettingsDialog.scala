package org.tubs.view

import scalafx.Includes.*
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.*
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.layout.*
import scalafx.scene.paint.Color

import org.tubs.settings.Settings

object SettingsDialog:

  /**
   * Displays the General Settings dialog and applies temporary changes if confirmed.
   */
  def show(): Unit =
    // Section 1: Grid & Dimensions
    val defaultColsSpinner   = new Spinner[Int](1, 1000, Settings.defaultCols.value) { editable = true; prefWidth = 120 }
    val defaultRowsSpinner   = new Spinner[Int](1, 1000, Settings.defaultRows.value) { editable = true; prefWidth = 120 }
    val prelookupSpinner     = new Spinner[Int](1, 100, Settings.prelookupNum.value) { editable = true; prefWidth = 120 }

    val cellWidthSpinner     = new Spinner[Double](10.0, 500.0, Settings.cellWidth.value, 5.0) { editable = true; prefWidth = 120 }
    val cellHeightSpinner    = new Spinner[Double](10.0, 200.0, Settings.cellHeight.value, 2.0) { editable = true; prefWidth = 120 }
    val minCellWidthSpinner  = new Spinner[Double](5.0, 200.0, Settings.minCellWidth.value, 5.0) { editable = true; prefWidth = 120 }
    val minCellHeightSpinner = new Spinner[Double](5.0, 100.0, Settings.minCellHeight.value, 1.0) { editable = true; prefWidth = 120 }

    val gridDimensions = new GridPane:
      hgap = 12
      vgap = 8
      padding = Insets(4, 10, 4, 10)

    def addGridRow(grid: GridPane, row: Int, labelText: String, control: Control): Unit =
      val lbl = new Label(labelText) { minWidth = 150; alignment = Pos.CenterLeft }
      grid.add(lbl, 0, row)
      grid.add(control, 1, row)

    addGridRow(gridDimensions, 0, "Default Columns:", defaultColsSpinner)
    addGridRow(gridDimensions, 1, "Default Rows:", defaultRowsSpinner)
    addGridRow(gridDimensions, 2, "Prelookup Margin:", prelookupSpinner)
    addGridRow(gridDimensions, 3, "Cell Width:", cellWidthSpinner)
    addGridRow(gridDimensions, 4, "Cell Height:", cellHeightSpinner)
    addGridRow(gridDimensions, 5, "Min Cell Width:", minCellWidthSpinner)
    addGridRow(gridDimensions, 6, "Min Cell Height:", minCellHeightSpinner)

    // Section 2: Zoom Controls
    val minZoomSpinner  = new Spinner[Double](0.1, 1.0, Settings.minZoomScale.value, 0.1) { editable = true; prefWidth = 120 }
    val maxZoomSpinner  = new Spinner[Double](1.0, 5.0, Settings.maxZoomScale.value, 0.5) { editable = true; prefWidth = 120 }
    val zoomStepSpinner = new Spinner[Double](0.01, 1.0, Settings.zoomStep.value, 0.05) { editable = true; prefWidth = 120 }

    val zoomGrid = new GridPane:
      hgap = 12
      vgap = 8
      padding = Insets(4, 10, 4, 10)

    addGridRow(zoomGrid, 0, "Min Zoom Scale:", minZoomSpinner)
    addGridRow(zoomGrid, 1, "Max Zoom Scale:", maxZoomSpinner)
    addGridRow(zoomGrid, 2, "Zoom Step:", zoomStepSpinner)

    // Section 3: Appearance & Colors
    val defaultFgPicker      = new ColorPicker(Settings.defaultFgColor.value) { prefWidth = 120 }
    val defaultBgPicker      = new ColorPicker(Settings.defaultBgColor.value) { prefWidth = 120 }
    val headerBgPicker       = new ColorPicker(Color.web(Settings.headerBgColor.value)) { prefWidth = 120 }
    val headerActiveBgPicker = new ColorPicker(Color.web(Settings.headerActiveBgColor.value)) { prefWidth = 120 }
    val headerFgPicker       = new ColorPicker(Color.web(Settings.headerFgColor.value)) { prefWidth = 120 }
    val headerActiveFgPicker = new ColorPicker(Color.web(Settings.headerActiveFgColor.value)) { prefWidth = 120 }
    val headerBorderPicker   = new ColorPicker(Color.web(Settings.headerBorderColor.value)) { prefWidth = 120 }
    val selectedBorderPicker = new ColorPicker(Color.web(Settings.selectedBorderColor.value)) { prefWidth = 120 }
    val normalBorderPicker   = new ColorPicker(Color.web(Settings.normalBorderColor.value)) { prefWidth = 120 }
    val selectionPillPicker  = new ColorPicker(Color.web(Settings.selectionPillBgColor.value)) { prefWidth = 120 }

    val appearanceGrid = new GridPane:
      hgap = 12
      vgap = 8
      padding = Insets(4, 10, 4, 10)

    addGridRow(appearanceGrid, 0, "Text Color:", defaultFgPicker)
    addGridRow(appearanceGrid, 1, "Cell Background:", defaultBgPicker)
    addGridRow(appearanceGrid, 2, "Header Background:", headerBgPicker)
    addGridRow(appearanceGrid, 3, "Active Header BG:", headerActiveBgPicker)
    addGridRow(appearanceGrid, 4, "Header Text Color:", headerFgPicker)
    addGridRow(appearanceGrid, 5, "Active Header Text:", headerActiveFgPicker)
    addGridRow(appearanceGrid, 6, "Header Border Color:", headerBorderPicker)
    addGridRow(appearanceGrid, 7, "Selected Cell Border:", selectedBorderPicker)
    addGridRow(appearanceGrid, 8, "Normal Cell Border:", normalBorderPicker)
    addGridRow(appearanceGrid, 9, "Selection Pill BG:", selectionPillPicker)

    def sectionLabel(text: String): Label =
      new Label(text):
        style = """
        | -fx-font-weight: bold;
        | -fx-font-size: 13px;
        | -fx-text-fill: #2c3e50;
        """.stripMargin

    val form = new VBox:
      spacing = 10
      padding = Insets(10)
      children = Seq(
        sectionLabel("Grid & Dimensions"),
        gridDimensions,
        new Separator(),
        sectionLabel("Zoom Controls"),
        zoomGrid,
        new Separator(),
        sectionLabel("Appearance & Colors"),
        appearanceGrid
      )

    val scrollPane = new ScrollPane:
      content = form
      fitToWidth = true
      prefHeight = 460
      maxHeight = 600

    val applyButtonType = new ButtonType("Apply", ButtonData.OKDone)
    val cancelButtonType = new ButtonType("Cancel", ButtonData.CancelClose)

    val dialog = new Dialog[Boolean]:
      title = "Settings"
      headerText = "General Settings"
      dialogPane = new DialogPane:
        content = scrollPane
        buttonTypes = Seq(applyButtonType, cancelButtonType)
      resultConverter = bt => bt == applyButtonType

    dialog.showAndWait() match
      case Some(true) =>
        Settings.defaultCols.value = defaultColsSpinner.value.value
        Settings.defaultRows.value = defaultRowsSpinner.value.value
        Settings.prelookupNum.value = prelookupSpinner.value.value

        Settings.cellWidth.value = cellWidthSpinner.value.value
        Settings.cellHeight.value = cellHeightSpinner.value.value
        Settings.minCellWidth.value = minCellWidthSpinner.value.value
        Settings.minCellHeight.value = minCellHeightSpinner.value.value

        Settings.minZoomScale.value = minZoomSpinner.value.value
        Settings.maxZoomScale.value = maxZoomSpinner.value.value
        Settings.zoomStep.value = zoomStepSpinner.value.value

        Settings.defaultFgColor.value       = defaultFgPicker.value.value
        Settings.defaultBgColor.value       = defaultBgPicker.value.value
        Settings.headerBgColor.value        = View.toHex(headerBgPicker.value.value)
        Settings.headerActiveBgColor.value  = View.toHex(headerActiveBgPicker.value.value)
        Settings.headerFgColor.value        = View.toHex(headerFgPicker.value.value)
        Settings.headerActiveFgColor.value  = View.toHex(headerActiveFgPicker.value.value)
        Settings.headerBorderColor.value    = View.toHex(headerBorderPicker.value.value)
        Settings.selectedBorderColor.value  = View.toHex(selectedBorderPicker.value.value)
        Settings.normalBorderColor.value    = View.toHex(normalBorderPicker.value.value)
        Settings.selectionPillBgColor.value = View.toHex(selectionPillPicker.value.value)

      case _ =>

