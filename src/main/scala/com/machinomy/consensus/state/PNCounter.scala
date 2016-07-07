package com.machinomy.consensus.state

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

  def update(key: K, newValue: E): Self = {
    val num = implicitly[Numeric[E]]
    val delta = num.minus(newValue, get(key))
    increment(key, delta)
  }

  def get(key: K): E = {
    val num = implicitly[Numeric[E]]
    val increment = increments.get(key)
    val decrement = decrements.get(key)
    num.minus(increment, decrement)
  }

  def table: Map[K, E] = {
    val keys = increments.state.keys.toSet ++ decrements.state.keys.toSet
    keys.foldLeft(Map.empty[K, E]) { (acc, k) =>
        acc.updated(k, get(k))
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
