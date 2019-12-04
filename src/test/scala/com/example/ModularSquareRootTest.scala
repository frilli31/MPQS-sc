package com.example

import com.example.ModularSquareRoot.modular_sqrt
import org.scalatest.FunSuite

class ModularSquareRootTest extends FunSuite {
  // test 1
  test("Modular Square Root of 23479349 and 23") {
    val ris = modular_sqrt(BigInt(23479349), BigInt(23))
    assert(ris == BigInt(12))
  }
}
