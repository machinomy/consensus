package com.machinomy.consensus.state

case class PNCounter[K, E: Numeric](increments: GCounter[K, E], decrements: GCounter[K, E]) extends Convergent[E, E] {
  override type Self = PNCounter[K, E]

  def +(i: (K, E)): Self = update(i._1, i._2)

  def update(key: K, newValue: E): Self = {
    copy(increments = increments + (key, newValue))
  }

  def get(key: K): E = {
    /*val num = implicitly[Numeric[E]]
    val increment = increments.get(key)
    val decrement = decrements.get(key)
    num.minus(increment, decrement)*/
    increments.get(key)
  }

  def table: Map[K, E] = {
    increments.state
  }

  override def merge(other: Self): Self = {
    println(s"MERGE::: ${increments.state} ++ ${other.increments.state}")
    val incrs = increments.state ++ other.increments.state
    println(s"MERGE::: ${increments.state} ++ ${other.increments.state} == $incrs")
    new PNCounter[K, E](new GCounter[K, E](incrs), decrements)
  }

  override def value: E = {
    increments.value
  }
}

object PNCounter {
  def apply[K, E: Numeric](): PNCounter[K, E] = new PNCounter(GCounter[K, E](), GCounter[K, E]())
}
