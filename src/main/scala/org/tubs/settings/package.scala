
package org.tubs.settings

import scalafx.beans.property.{DoubleProperty, IntegerProperty, ObjectProperty, StringProperty}
import scalafx.scene.paint.Color

/**
 * Shared configuration settings, colors, dimensions, and limits for tubs.
 */
object Settings:
  val defaultFgColor = ObjectProperty[Color](Color.rgb(32, 39, 52))
  val defaultBgColor = ObjectProperty[Color](Color.White)

  val gridBgColor = StringProperty("#ffffff")
  val selectionPillBgColor = StringProperty("#e9ecef")
  val selectedBorderColor = StringProperty("#1a73e8")
  val normalBorderColor = StringProperty("#d0d7de")
  val headerBgColor = StringProperty("#f1f3f5")
  val headerActiveBgColor = StringProperty("#d0d7de")
  val headerBorderColor = StringProperty("#ced4da")

  val defaultCols = IntegerProperty(26)
  val defaultRows = IntegerProperty(50)
  val prelookupNum = IntegerProperty(10)

  val cellWidth = DoubleProperty(80.0)
  val cellHeight = DoubleProperty(26.0)

  val minCellWidth = DoubleProperty(40.0)
  val minCellHeight = DoubleProperty(13.0)

  val minZoomScale = DoubleProperty(0.5)
  val maxZoomScale = DoubleProperty(2.0)

  val zoomStep = DoubleProperty(0.1)

