package com.example

import com.example.ModularSquareRoot.{legendre, modular_sqrt}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.math.{log10, pow}

object QuadraticSieve {
  def qs(n: BigInt): Option[BigInt] = {
    val (roota, factorbase, tqsrt, xmax, tlog, thresh, min_prime) = initializeQS(n)
    sequentialSmooths(roota, n, factorbase, xmax, tqsrt, min_prime, tlog, thresh)
  }

  def initializeQS(n: BigInt) = {
    val root2n = sqrt(n * 2)

    val bound = (pow(log10(n.toDouble), 2) * 5).toInt

    val factorbase = makeSoE_PrimesTo(bound)
      .takeWhile(i => i <= bound)
      .filter(i => legendre(n, BigInt(i)) == 1 || i == 2)
      .toArray

    var (tqsrt, tlog) = factorbase
      .iterator
      .map(p => (modular_sqrt(n, p), log10(p.toDouble)))
      .toArray
      .unzip
    tqsrt(0) = 0
    val xmax = factorbase.length * 60 * 4
    val mval = (root2n * xmax) >> 1
    var thresh = log10(mval.toDouble) * 0.735
    val min_prime = BigInt((thresh * 3).toLong)
    val fudge = factorbase.iterator.takeWhile(p => p < min_prime)
      .zipWithIndex
      .map(t => tlog(t._2))
      .sum / 4
    thresh -= fudge

    var roota = sqrt(root2n / xmax)

    if (roota % 2 == 0)
      roota += 1

    roota = roota max 3
    (roota, factorbase, tqsrt, xmax, tlog, thresh, min_prime)
  }

  // Defining Sieve of Eratosthenes (from Rosetta Code)
  private def makeSoE_PrimesTo(top: Int): Iterator[Int] = {
    val topNdx = (top - 3) / 2 //odds composite BitSet buffer offset down to 3
    val cmpsts = new scala.collection.mutable.BitSet(topNdx + 1) //size includes topNdx
    @inline def cullPrmCmpsts(prmNdx: Int) = {
      val prm = prmNdx + prmNdx + 3;
      cmpsts ++= ((prm * prm - 3) >>> 1 to topNdx by prm)
    }

    (0 to (Math.sqrt(top).toInt - 3) / 2).filterNot {
      cmpsts
    }.foreach {
      cullPrmCmpsts
    }
    Iterator.single(2) ++ (0 to topNdx).filterNot {
      cmpsts
    }.map { pi => pi + pi + 3 }
  }

  // http://www.codecodex.com/wiki/Calculate_an_integer_square_root
  def sqrt(number: BigInt) = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1

    val one = BigInt(1)
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    while (n1 * n1 > number) {
      n1 -= one
    }
    n1
  }

  private def sequentialSmooths(
                                 rootaA: BigInt,
                                 n: BigInt,
                                 factorbase: Array[Int],
                                 xmax: Int,
                                 tsqrt: Array[BigInt],
                                 min_prime: BigInt,
                                 tlog: Array[Double],
                                 thresh: Double
                               ): Option[BigInt] = {
    var roota = rootaA
    val smooths: ArrayBuffer[(BigInt, (BigInt, BigInt))] = ArrayBuffer()
    val partials: mutable.HashMap[BigInt, (BigInt, (BigInt, BigInt))] = HashMap()
    val sievesize = 1 << 15

    while (smooths.length <= factorbase.length) {
      roota = nextPrime(roota)
      while (legendre(n, roota) != 1) {
        roota = nextPrime(roota)
      }
      val a = roota.pow(2)
      var b = modular_sqrt(n, roota)

      b = (b - (b * b - n) * (b * 2).modInverse(roota)) % a

      val c = (b * b - n) / a

      var s1: HashMap[Int, BigInt] = HashMap()
      var s2: HashMap[Int, BigInt] = HashMap()

      for ((p, i) <- factorbase.zipWithIndex) {
        val p_minus_2 = p - 2
        val ainv = a.modPow(p_minus_2, p)
        var sol1 = (tsqrt(i) - b) * ainv % p
        var sol2 = (-tsqrt(i) - b) * ainv % p
        sol1 -= ((sol1 + xmax) / p) * p
        sol2 -= ((sol2 + xmax) / p) * p

        s1.addOne((p, sol1 + xmax))
        s2.addOne((p, sol2 + xmax))
      }

      for (low <- 0 - xmax until xmax + 1 by sievesize + 1) {
        val high = xmax.min(low + sievesize)
        val size = high - low
        val size_plus_1 = size + 1

        val S: Array[Double] = Array.fill(size_plus_1)(0.toDouble)

        for ((p_i, i) <- factorbase.zipWithIndex) {
          if (p_i >= min_prime) {
            val p = p_i.toInt
            var sol1 = s1(p_i).toInt
            var sol2 = s2(p_i).toInt
            val logp = tlog(i)

            while (sol1 <= size || sol2 <= size) {
              if (sol1 <= size) {
                S(sol1) += logp
                sol1 += p
              }
              if (sol2 <= size) {
                S(sol2) += logp
                sol2 += p
              }
            }
            s1.addOne((p_i, sol1 - size_plus_1))
            s2.addOne((p_i, sol2 - size_plus_1))
          }
        }

        for (i <- 0 until size_plus_1) {
          if (S(i) > thresh) {
            val x = i + low
            val tofact = a * x * x + b * x * 2 + c
            var nf = tofact.abs

            for (p <- factorbase)
              while (nf % p == 0)
                nf /= p

            if (nf == 1) {
              smooths.addOne((a * x + b, (tofact, roota)))
            } else {
              partials.remove(nf) match {
                case Some((pairv, pairvals)) => smooths.addOne((
                  (a * x + b) * pairv,
                  (tofact * pairvals._1, roota * pairvals._2 * nf),
                ))
                case None => partials.addOne((nf, (a * x + b, (tofact, roota))))
              }
            }
          }
        }
      }
      if (smooths.length > factorbase.length) {
        algebra(factorbase, smooths.toArray, n) match {
          case Some(n) => return Some(n)
          case None => {}
        }
      }
    }
    None
  }

  def nextPrime(n: BigInt): BigInt = {
    var z = if (n % 2 == 0) n + 1 else n + 2
    while (!z.isProbablePrime(20))
      z += 2
    z
  }

  def algebra(factorbase: Array[Int], smooths: Array[(BigInt, (BigInt, BigInt))], settings: BigInt): Option[BigInt] = {
    val n = settings
    val mVector = smooths.iterator.map(i => createVector(i._2._1, factorbase)).toArray
    val factorbaseNew = Array(-1) concat factorbase
    val hVector = mVector.indices.map(i => BigInt(1) << i).toArray

    val (mVector2, hVector2) = reduceRowEchelonForm(mVector, hVector, factorbaseNew.length)

    val nulCols = hVector2.indices
      .filter(i => mVector2(i) == 0)
      .map(i => hVector2(i))

    for (nc <- nulCols) {
      var lhs = BigInt(1)
      val rhs = Array.fill(factorbaseNew.length)(BigInt(0))
      var rhspr = BigInt(1)

      for (index <- smooths.indices) {
        if (((BigInt(1) << index) & nc) > 0) {
          var (lh, (rh, ra)) = smooths(index)
          lhs *= lh
          rhspr *= ra
          if (rh < 0)
            rhs(0) += 1
          for (j <- 1 until factorbaseNew.length) {
            while (rh % factorbaseNew(j) == 0) {
              rh /= factorbaseNew(j)
              rhs(j) += 1
            }
          }
        }
      }
      for (j <- factorbaseNew.indices)
        rhspr *= BigInt(factorbaseNew(j)).pow(rhs(j).toInt >> 1)
      val g = (rhspr - lhs) gcd n
      if (g != 1 && g != n)
        return Some(g)
    }
    None
  }

  private def createVector(n_immutable: BigInt, factorbase: Array[Int]): BigInt = {
    var n = n_immutable
    var a = BigInt(0)
    val lg = factorbase.length - 1
    if (n < 0) {
      a |= (BigInt(2) << lg)
      n = -n
    }
    factorbase
      .zipWithIndex
      .filter(p => n % p._1 == 0)
      .foreach(tuple => {
        val (p, i) = tuple
        var c = 0
        while (n % p == 0) {
          n /= p
          c += 1
        }
        if ((c & 1) > 0)
          a |= (BigInt(1) << (lg - i))
      })
    a
  }

  private def reduceRowEchelonForm(mImm: Array[BigInt], hImm: Array[BigInt], columnCount: Int): (Array[BigInt], Array[BigInt]) = {
    if (mImm.isEmpty)
      (mImm, hImm)
    else {
      var lead = 0
      val rowCount = mImm.length

      var m = mImm
      var h = hImm

      for (r <- 0 until rowCount) {
        if (lead >= columnCount)
          return (m, h)
        var i = r
        while ((m(i) & (BigInt(1) << lead)) == 0) {
          i += 1
          if (i == rowCount) {
            i = r
            lead += 1
            if (columnCount == lead)
              return (m, h)
          }
        }

        val tmp1 = m(r)
        m(r) = m(i)
        m(i) = tmp1

        val tmp2 = h(r)
        h(r) = h(i)
        h(i) = tmp2

        for (index <- 0 until rowCount)
          if (index != r && ((BigInt(1) << lead) & m(index)) != 0) {
            m(index) ^= m(r)
            h(index) ^= h(r)
          }

        lead += 1
      }
      (m, h)
    }
  }

  def check_is_divisor(n: BigInt, qs: Option[BigInt]): Unit = {
    qs match {
      case Some(qs) => {
        val qr = n /% qs
        assert(qr._2 == BigInt(0))
        println(s"${n} = ${qs} * ${qr._1}")
      }
      case None => {
        assert(n.isProbablePrime(20))
      }
    }
  }
}