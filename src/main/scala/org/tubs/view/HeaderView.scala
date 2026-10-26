
package org.tubs.view

import scalafx.scene.control.Label
import scalafx.geometry.Pos

class HeaderView(val str: String):
  val label = new Label(str):
    prefWidth = Defaults.CellWidth
    prefHeight = Defaults.CellHeight
    minWidth = Defaults.MinCellWidth
    minHeight = Defaults.MinCellHeight
    alignment = Pos.Center
    style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"


