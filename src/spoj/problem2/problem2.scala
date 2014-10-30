/*
SPOJ
2. Prime Generator
Problem code: PRIME1

Peter wants to generate some prime numbers for his crypto system.
Help him! Your task is to generate all prime numbers
between two given numbers!

Input
The input begins with the number t of test cases in a single line (t<=10).
In each of the next t lines, there are two numbers m and n
(1 <= m <= n <= 1000000000. n-m<=100000) separated by a space.

Output
For every test case, print all prime numbers p
such that m <= p <= n, one number per line,
test cases separated by an empty line.
*/
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer
import math.sqrt
import math.ceil

object Main {
  def main(args: Array[String]) {
    // first line has number of test cases
    val t = readInt()
    var testCases = new ListBuffer[List[Int]]()
    // next t lines, two numbers m and n separated by space
    for (i <- 1 to t) {
      // read in m and n
      var line = readLine().split(" +")
      val m = line(0).toInt
      val n = line(1).toInt
      testCases += List(m,n)
    }
    for (x <- testCases.toList) {
      // calculate primes
      calcPrimes(x(0),x(1))
      // blank line to separate test cases
      println()
    }
  }

  // calculate primes between m and n
  def calcPrimes(m: Int, n: Int) = {

    def isPrime(x: Int): Boolean = {
      var prime = true
      if (x > 3) {
        for (i <- 2 to sqrt(x.toDouble).ceil.toInt) {
          if (x % i == 0) prime = false
        }
      }
      prime
    }

    for (i <- m to n) {
      if (isPrime(i)) println(i)
    }
  }

}
