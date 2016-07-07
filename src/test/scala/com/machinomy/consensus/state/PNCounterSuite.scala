package com.machinomy.consensus.state

import org.scalatest.FunSuite

class PNCounterSuite extends FunSuite {
  test("Empty value is zero") {
    val fresh = PNCounter[Int, Int]()
    assert(fresh.value === 0)
  }

  test("Could be calculated") {
    val counter = PNCounter[Int, Int]().increment(1, 1).increment(2, 3)
    assert(counter.value === 1 + 3)
  }

  test("Could be merged") {
    val a = PNCounter[Int, Int]().increment(1, 1).increment(2, 3)
    val b = PNCounter[Int, Int]().increment(1, 2).increment(2, -3)
    val c = a.merge(b)
    assert(c.value === 2)
  }

  test("Could get replica value") {
    val a = PNCounter[Int, Int]().increment(1, 2).increment(2, 3).increment(1, -1)
    assert(a.get(1) === 1)
  }
}
