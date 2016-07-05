package com.machinomy.consensus.state

trait Convergent[Element, Value] {
  type Self <: Convergent[Element, Value]
  def merge(other: Self): Self
  def value: Value
}
