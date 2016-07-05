package com.machinomy.consensus.counters

import org.scalatest.FunSuite

class GCounterSuite extends FunSuite {
  test("Fresh GCounter is empty") {
    val fresh = new GCounter[Int, Int]()
    assert(fresh.state.isEmpty)
  }

  test("GCounter could be calculated") {
    val counter = new GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    assert(counter.value === 5)
  }

  test("GCounter could be merged") {
    val a = new GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    val b = new GCounter[Int, Int]().increment(1, 2).increment(3, 4)
    val c = a.merge(b)
    assert(c.value === 2 + 3 + 4)
  }
}
