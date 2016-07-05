package com.machinomy.consensus.state

/**
  * 2P-Set
  */
case class TPSet[E](additions: Set[E], removals: Set[E]) extends Convergent[E, Set[E]] {
  override type Self = TPSet[E]

  def +(e: E): Self = copy(additions = additions + e)

  def -(e: E): Self =
    if (additions.contains(e)) {
      copy(removals = removals + e)
    } else {
      this
    }

  override def merge(other: Self): Self = TPSet(additions ++ other.additions, removals ++ other.removals)

  override def value: Set[E] = additions -- removals
}

object TPSet {
  def apply[E](): TPSet[E] = TPSet(Set.empty[E], Set.empty[E])
}
