
package org.tubs.view

import scalafx.scene.control.Label
import scalafx.geometry.Pos

import org.tubs.settings.Settings

class HeaderView(val str: String):
  val label = new Label(str):
    prefWidth = Settings.cellWidth.value
    prefHeight = Settings.cellHeight.value
    minWidth = Settings.minCellWidth.value
    minHeight = Settings.minCellHeight.value
    alignment = Pos.Center
    style = s"-fx-background-color: ${Settings.headerBgColor.value}; -fx-border-color: ${Settings.headerBorderColor.value}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"


