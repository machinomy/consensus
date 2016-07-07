package com.machinomy.consensus.state

case class GCounter[K, E : Numeric](state: Map[K, E] = Map.empty[K, E]) extends Convergent[E, E] {
  override type Self = GCounter[K, E]

  def +(i: (K, E)): Self = increment(i._1, i._2)

  def increment(key: K, delta: E): Self = {
    val num = implicitly[Numeric[E]]
    require(num.gteq(delta, num.zero), "Can only increment GCounter")

    if (num.equiv(delta, num.zero)) {
      this
    } else {
      state.get(key) match {
        case Some(value) => new GCounter[K, E](state.updated(key, num.plus(value, delta)))
        case None => new GCounter[K, E](state.updated(key, delta))
      }
    }
  }

  def get(key: K): E = {
    val num = implicitly[Numeric[E]]
    state.getOrElse(key, num.zero)
  }

  override def merge(other: Self): Self = {
    val num = implicitly[Numeric[E]]
    val nextState = state.foldLeft(other.state) { case (acc, (key, value)) =>
      if (num.gt(value, acc.getOrElse(key, num.zero))) {
        acc.updated(key, value)
      } else {
        acc
      }
    }
    new GCounter(nextState)
  }

  override def value: E = state.values.sum
}
