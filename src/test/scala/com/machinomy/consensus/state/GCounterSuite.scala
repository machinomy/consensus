package com.machinomy.consensus.state

import org.scalatest.FunSuite

class GCounterSuite extends FunSuite {
  test("Fresh GCounter is empty") {
    val fresh = GCounter[Int, Int]()
    assert(fresh.state.isEmpty)
  }

  test("GCounter could be calculated") {
    val counter = GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    assert(counter.value === 5)
  }

  test("GCounter could be merged") {
    val a = GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    val b = GCounter[Int, Int]().increment(1, 2).increment(3, 4)
    val c = a.merge(b)
    assert(c.value === 2 + 3 + 4)
  }

  test("GCounter can get replica value") {
    val a = GCounter[Int, Int]()
    assert(a.get(0) === 0)
    val b = a.increment(1, 2).increment(2, 3)
    assert(b.get(1) === 2)
    assert(b.get(2) === 3)
  }
}
