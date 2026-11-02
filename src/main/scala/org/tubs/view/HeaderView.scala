
package org.tubs.view

import scalafx.scene.control.Label
import scalafx.geometry.Pos

import org.tubs.settings.Settings

class HeaderView(val str: String):
  val label = new Label(str):
    prefWidth <== Settings.cellWidth
    prefHeight <== Settings.cellHeight
    minWidth <== Settings.minCellWidth
    minHeight <== Settings.minCellHeight
    alignment = Pos.Center
    style = s"-fx-background-color: ${Settings.headerBgColor.value}; -fx-border-color: ${Settings.headerBorderColor.value}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: ${Settings.headerFgColor.value};"


