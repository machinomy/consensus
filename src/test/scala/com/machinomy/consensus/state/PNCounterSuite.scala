package com.machinomy.consensus.state

import org.scalatest.FunSuite

class PNCounterSuite extends FunSuite {
  test("Empty PNCounter value is zero") {
    val fresh = PNCounter[Int, Int]()
    assert(fresh.value === 0)
  }

  test("PNCounter could be calculated") {
    val counter = PNCounter[Int, Int]().increment(1, 1).increment(2, 3)
    assert(counter.value === 1 + 3)
  }

  test("PNCounter could be merged") {
    val a = PNCounter[Int, Int]().increment(1, 1).increment(2, 3)
    val b = PNCounter[Int, Int]().increment(1, 2).increment(2, -3)
    val c = a.merge(b)
    assert(c.value === 2)
  }
}
