
package org.tubs.view

import scalafx.scene.paint.Color

/**
 * Shared default appearance values, colors, dimensions and configurations for the View.
 */
object Defaults:
  val DefaultFgColor: Color = Color.rgb(32, 39, 52)
  val DefaultBgColor: Color = Color.White
  val GridBgColor: String = "#ffffff"
  val SelectionPillBgColor: String = "#e9ecef"
  val SelectedBorderColor: String = "#1a73e8"
  val NormalBorderColor: String = "#d0d7de"
  val HeaderBgColor: String = "#f1f3f5"
  val HeaderActiveBgColor: String = "#d0d7de"
  val HeaderBorderColor: String = "#ced4da"

  val DefaultCols: Int = 26
  val DefaultRows: Int = 50
  val PrelookupNum: Int = 10

  val CellWidth: Double = 80.0
  val CellHeight: Double = 26.0
  val MinCellWidth: Double = 40.0
  val MinCellHeight: Double = 13.0
  val RowHeaderWidth: Double = 40.0
  val ColHeaderHeight: Double = 24.0

  val MinZoomScale: Double = 0.5
  val MaxZoomScale: Double = 2.0
  val ZoomStep: Double = 0.1

