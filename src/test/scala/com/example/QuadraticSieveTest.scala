package com.example

import com.example.QuadraticSieve.{check_is_divisor, nextPrime, qs}
import org.scalatest.FunSuite

class QuadraticSieveTest extends FunSuite {
  // test 1
  test("Next Prime") {
    val n = BigInt("523022617466601111760007224100074291200000001")
    val r = nextPrime(n)
    assert(r.isProbablePrime(2000))
  }

  test("QS  9986801107") {
    val n = BigInt("9986801107")
    val ris = qs(n)
    check_is_divisor(n, ris)
  }

  // test 2
  test("QS  523022617466601111760007224100074291200000001") {
    val n = BigInt("523022617466601111760007224100074291200000001")
    val ris = qs(n)
    check_is_divisor(n, ris)
  }
}
