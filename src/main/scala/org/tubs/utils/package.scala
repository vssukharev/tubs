
package org.tubs.utils

import upickle.default.*

object Todo extends Throwable:
  def apply(s: String) = throw new Exception("Todo: " + s)
  def apply() = throw new Exception("Todo")

object Unreachable extends Throwable:
  val msg = "Unreachable statement reached"
  def apply(s: String) = throw new Exception(msg + " from " + s)
  def apply() = throw new Exception(msg)

