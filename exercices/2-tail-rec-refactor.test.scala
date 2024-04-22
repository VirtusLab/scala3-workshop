//> using toolkit default

package ex2

import munit.Assertions
import munit.Tag
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// Convert functions to use tail recursion
def factorial(n: BigInt): BigInt = ???

def fibonaci(n: BigInt): BigInt = ???

class TailRecRefactorSuite extends munit.FunSuite {
  test("Factorial - Gives correct answers for small integers") {
    assert(factorial(0) == 1)
    assert(factorial(1) == 1)
    assert(factorial(2) == 2)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
  }

  test("Factorial - Handles large integers without StackOverflow") {
    try factorial(1000)
    catch { case _: StackOverflowError => fail("should not throw StackOverflowException") }
  }

  // Fibonacii
  test("Fibonaci: Gives correct answers for small integers") {
    assert(fibonaci(0) == 1)
    assert(fibonaci(1) == 1)
    assert(fibonaci(2) == 2)
    assert(fibonaci(3) == 3)
    assert(fibonaci(4) == 5)
    assert(fibonaci(5) == 8)
  }

  test("Fibonaci : Handles large integers without StackOverflow") {
    Future:
      try fibonaci(1000)
      catch { case _: StackOverflowError => fail("should not throw StackOverflowException") }
  }
}
