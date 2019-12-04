package com.example

object ModularSquareRoot {
  def modular_sqrt(n: BigInt, p: BigInt): BigInt = {
    if (legendre(n, p) != 1 || n == 0)
      0
    else if (p == 2)
      p
    else if (p.mod(4) == 3)
      n.modPow((p + 1) / 4, p)
    else {
      var s = p - 1
      var e = 0

      while (s % 2 == 0) {
        s /= 2
        e += 1
      }

      var z = BigInt(2);

      while (legendre(z, p) != -1)
        z += 1

      var x = n.modPow((s + 1) / 2, p)
      var b = n.modPow(s, p)
      var g = z.modPow(s, p)
      var r = e

      while (true) {
        var t = b
        var m = 0

        while (m < r && t != 1) {
          t = t.modPow(2, p)
          m += 1
        }
        if (m == 0)
          return x

        val gs = g.modPow(BigInt(2).pow(r - m - 1), p)
        g = gs * gs % p
        x = x * gs % p
        b = b * g % p
        r = m
      }
      1
    }
  }

  def legendre(a: BigInt, p: BigInt): BigInt = {
    val ls = a.modPow((p - 1) / 2, p);
    if (ls == p - 1)
      -1
    else
      ls
  }
}
