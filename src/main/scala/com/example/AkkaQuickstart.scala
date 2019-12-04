package com.example

//#main-class
object AkkaQuickstart {
  def main(args: Array[String]): Unit = {
    val n = args.tail.map(i => BigInt(i)).product
    if (args(0) equals "P")
      time {
        println(ParallelMPQS.qs(n))
      }
    else
      time {
        println(QuadraticSieve.qs(n))
      }
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}
