package com.machinomy.consensus.state.counters

import com.machinomy.consensus.state.Convergent

case class PNCounter[K, E: Numeric](increments: GCounter[K, E], decrements: GCounter[K, E]) extends Convergent[E, E] {
  override type Self = PNCounter[K, E]

  def +(i: (K, E)): Self = increment(i._1, i._2)

  def increment(key: K, delta: E): Self = {
    val num = implicitly[Numeric[E]]
    if (num.gt(delta, num.zero)) {
      copy(increments = increments.increment(key, delta))
    } else if (num.lt(delta, num.zero)) {
      copy(decrements = decrements.increment(key, num.negate(delta)))
    } else {
      this
    }
  }

  override def merge(other: Self): Self =
    copy(increments = other.increments.merge(increments), decrements = other.decrements.merge(decrements))

  override def value: E = {
    val num = implicitly[Numeric[E]]
    num.minus(increments.value, decrements.value)
  }
}

object PNCounter {
  def apply[K, E: Numeric](): PNCounter[K, E] = new PNCounter(GCounter[K, E](), GCounter[K, E]())
}
