package com.machinomy.consensus.state

import org.scalatest.FunSuite

class GSetSuite extends FunSuite {
  test("Just created GSet is empty") {
    val gSet = new GSet[Int]()
    assert(gSet.value.isEmpty)
  }

  test("GSet calculates value") {
    val gSet0 = new GSet[Int]()
    val gSet1 = gSet0 + 3
    val gSet2 = gSet1 + 1
    val gSet3 = gSet2 + 3
    assert(gSet3.value === Set(1, 3))
  }

  test("GSets can be merged") {
    val gSetA = new GSet[Int](Set(1, 2, 3))
    val gSetB = new GSet[Int](Set(2, 3, 4))
    val result = gSetA.merge(gSetB)
    assert(result.value === Set(1, 2, 3, 4))
  }
}
