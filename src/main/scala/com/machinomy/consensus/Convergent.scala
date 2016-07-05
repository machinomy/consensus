package com.machinomy.consensus

trait Convergent[Element, Value] {
  type Self <: Convergent[Element, Value]
  def merge(other: Self): Self
  def value: Value
}
