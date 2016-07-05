package com.machinomy.consensus.sets

import com.machinomy.consensus.Convergent

class GSet[E](set: Set[E] = Set.empty[E]) extends Convergent[E, Set[E]] {
  override type Self = GSet[E]

  def +(element: E) = new GSet[E](set + element)

  override def merge(other: Self): Self = new GSet[E](value ++ other.value)

  override def value: Set[E] = set
}
