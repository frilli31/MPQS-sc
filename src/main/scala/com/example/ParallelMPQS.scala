package com.example

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Kill, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.ModularSquareRoot.{legendre, modular_sqrt}
import com.example.QuadraticSieve.{algebra, initializeQS, nextPrime}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.concurrent.Await

class Combiner(rootaA: BigInt, parent: ActorRef, n: BigInt, factorbase: Array[Int]) extends Actor {
  var roota = rootaA
  var smooths: ArrayBuffer[(BigInt, (BigInt, BigInt))] = mutable.ArrayBuffer()
  var partials: mutable.HashMap[BigInt, (BigInt, (BigInt, BigInt))] = mutable.HashMap()

  override def receive: Receive = {
    case (sm: Array[(BigInt, (BigInt, BigInt))], part: mutable.HashMap[BigInt, (BigInt, (BigInt, BigInt))]) => {
      smooths appendAll sm

      part.foreach {
        case (key, (pairv2, pairvals2)) => {
          partials.remove(key) match {
            case Some((pairv, pairvals)) => smooths addOne(pairv * pairv2, (pairvals2._1 * pairvals._1, pairvals._2 * pairvals2._2 * key))
            case None => partials addOne(key, (pairv2, pairvals2))
          }
        }
      }
      if (smooths.length > factorbase.length)
        algebra(factorbase, smooths.toArray, n) match {
          case Some(n) => parent ! Some(n)
          case None => {}
        }
      roota = nextPrime(roota)
      while (legendre(n, roota) != 1) {
        roota = nextPrime(roota)
      }
      sender ! roota
    }
  }
}

class Siever(
              n: BigInt,
              factorbase: Array[Int],
              xmax: Int,
              tqsrt: Array[BigInt],
              min_prime: BigInt,
              tlog: Array[Double],
              thresh: Double,
              combiner: ActorRef
            ) extends Actor {
  val sievesize = 1 << 15

  override def receive: Receive = {
    case roota: BigInt => {
      val smooths: ArrayBuffer[(BigInt, (BigInt, BigInt))] = ArrayBuffer()
      val partials: mutable.HashMap[BigInt, (BigInt, (BigInt, BigInt))] = HashMap()

      val a = roota.pow(2)
      var b = modular_sqrt(n, roota)

      b = (b - (b * b - n) * (b * 2).modInverse(roota)) % a

      val c = (b * b - n) / a

      var s1: HashMap[Int, BigInt] = HashMap()
      var s2: HashMap[Int, BigInt] = HashMap()

      for ((p, i) <- factorbase.zipWithIndex) {
        val p_minus_2 = p - 2
        val ainv = a.modPow(p_minus_2, p)
        var sol1 = (tqsrt(i) - b) * ainv % p
        var sol2 = (-tqsrt(i) - b) * ainv % p
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
      combiner ! (smooths.toArray, partials)
    }
  }
}

class SysAdmin(rootaA: BigInt,
               n: BigInt,
               factorbase: Array[Int],
               xmax: Int,
               tsqrt: Array[BigInt],
               min_prime: BigInt,
               tlog: Array[Double],
               thresh: Double) extends Actor {
  var respondTo: Option[ActorRef] = None

  override def receive: Receive = {
    case unit: Unit => {
      var roota: BigInt = rootaA
      respondTo = Some(sender())

      var rootas: ArrayBuffer[BigInt] = ArrayBuffer()
      for (i <- 0 until Runtime.getRuntime.availableProcessors * 2) {
        roota = nextPrime(roota)
        while (legendre(n, roota) != 1) {
          roota = nextPrime(roota)
        }
        rootas += roota
      }

      val combiner = context.actorOf(Props(classOf[Combiner], roota, self, n, factorbase), "combiner")
      for (i <- 0 until Runtime.getRuntime.availableProcessors) {
        val siever = context.actorOf(Props(classOf[Siever], n,
          factorbase,
          xmax,
          tsqrt,
          min_prime,
          tlog,
          thresh,
          combiner), name = "siever" + i.toString)
        siever ! rootas(i * 2)
        siever ! rootas(i * 2 + 1)
      }
    };
    case ris: Option[BigInt] => respondTo match {
      case Some(ref) => ref ! ris
      case None => {}
    }
  }
}

object ParallelMPQS {
  def qs(n: BigInt): Option[BigInt] = {
    val (roota, factorbase, tsqrt, xmax, tlog, thresh, min_prime) = initializeQS(n)

    val system = ActorSystem("Parallel-MPQS")

    val admin = system.actorOf(Props(classOf[SysAdmin], roota, n,
      factorbase,
      xmax,
      tsqrt,
      min_prime,
      tlog,
      thresh), name = "Admin")

    implicit val timeout = Timeout(1, TimeUnit.HOURS)
    val future = admin ? ()
    val result = Await.result(future, timeout.duration).asInstanceOf[Option[BigInt]]
    admin ! Kill
    system.terminate()
    result
  }
}
