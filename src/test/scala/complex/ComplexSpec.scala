package complex

import org.scalatest._
import math._

class ComplexSpec extends FlatSpec with Matchers {
  val smallError = 0.000001

  "Complex" should "support rectangular instantiation" in {
    val re = 1
    val im = 3
    val z = Complex.xy(re, im)
    z.re shouldEqual re
    z.im shouldEqual im
  }

  it should "support polar instantiation" in {
    val r = 1
    val theta = math.Pi
    val z = Complex.polar(r, theta)
    z.mag shouldEqual r
    z.arg shouldEqual theta
  }

  it should "convert to rectangular coordinates" in {
    val z = Complex.polar(2, -9*math.Pi/4)
    val sqrt2 = math.sqrt(2)
    z.re shouldBe (sqrt2 +- smallError)
    z.im shouldBe (-sqrt2 +- smallError)
  }

  it should "convert to polar coordinates" in {
    val sqrt2 = math.sqrt(2)
    val z = Complex.xy(sqrt2, -sqrt2)
    z.mag shouldBe (2.0 +- smallError)
    z.arg shouldBe (-math.Pi/4 +- smallError)
  }

  it should "support equality comparison" in {
    Complex.xy(0, 1) should equal (Complex.xy(0, 1))
    Complex.xy(0, 1) should not equal (Complex.xy(1, 1))
    Complex.xy(1, 0) should equal (Complex.polar(1, 0))
    Complex.xy(0, 1) should equal (Complex.polar(1, math.Pi/2))
    // standard test for floating point handlers (https://xkcd.com/217/)
    val big = 1000000000000.0
    Complex.xy(math.exp(math.Pi) - math.Pi, big) should not equal (Complex.xy(20, big))
  }

  "Complex arithmetic" should "include addition" in {
    Complex.xy(0, 1) + Complex.xy(0, -1) shouldEqual Complex.xy(0, 0)
    val z = Complex.polar(1, math.Pi/7)
    (z + z + z) shouldEqual Complex.polar(3, math.Pi/7)
  }

  it should "include negation" in {
    val a = Complex.xy(10, 20)
    -a shouldEqual Complex.xy(-10, -20)
    (a + -a) shouldEqual Complex.xy(0, 0)

    val b = Complex.polar(15, math.Pi/3)
    -b shouldEqual Complex.polar(15, 4*math.Pi/3)
    (b + -b) shouldEqual Complex.xy(0, 0)
  }

  it should "include multiplication" in {
    Complex.xy(3, 0) * Complex.xy(5, 0) shouldEqual Complex.xy(15, 0) // real *
    val i = Complex.polar(1, math.Pi/2)
    i * i shouldEqual Complex.xy(-1, 0) // i^2 = -1
    Complex.polar(1, math.Pi/5) * Complex.polar(0.5, math.Pi/3) shouldEqual Complex.polar(0.5, math.Pi/5 + math.Pi/3)
    Complex.xy(3, 1) * Complex.xy(-2, 5) shouldBe Complex.xy(-6 - 5, -2 + 15)
  }

  it should "include division" in {
    Complex.xy(3, 0) / Complex.xy(5, 0) shouldEqual Complex.xy(3/5.0, 0)
    Complex.polar(2, math.Pi) / Complex.polar(5, math.Pi/2) shouldEqual Complex.polar(2/5.0, math.Pi/2)
    Complex.xy(1, 2) / Complex.xy(1, 1) shouldEqual (Complex.xy(1, 2) * Complex.xy(1, -1)) / Complex.xy(2, 0)
  }

  it should "convert numbers implicitly" in {
    "Complex.xy(3, 4) + 5" should compile
    "1.3132 + Complex.xy(0, 0)" should compile
    "List(3) + Complex.xy(0, 0)" shouldNot compile
    Complex.xy(1, 2) + 7 shouldEqual Complex.xy(8, 2)
    Complex.i shouldEqual Complex.xy(0, 1)
    "1 + Complex.i*3" should compile
  }

  "Complex operations" should "provide log and e^" in {
    Complex.log(1) shouldEqual 0
    Complex.log(12.3134) shouldEqual math.log(12.3134)
    Complex.log(Complex.polar(1, math.Pi/2)) shouldEqual Complex.i*math.Pi/2
    Complex.exp(0) shouldEqual 1
    Complex.exp(math.log(12.3134)) shouldEqual 12.3134
    Complex.exp(Complex.i*math.Pi/2) shouldEqual Complex.polar(1, math.Pi/2)
    Complex.exp(Complex.log(Complex.xy(3, 5))) shouldEqual Complex.xy(3, 5)
    Complex.log(Complex.exp(Complex.xy(3, 5))) shouldEqual Complex.xy(3, 5 - 2*math.Pi)  // Note the weirdness
    Complex.log(Complex.exp(Complex.polar(5, math.Pi/7))) shouldEqual Complex.polar(5, math.Pi/7)
  }

  it should "provide exponentiation" in {
    Complex.pow(1, 3) shouldEqual 1
    Complex.pow(Complex.i, 3) shouldEqual -Complex.i
    Complex.pow(Complex.polar(3, math.Pi/4), 0.5) shouldEqual Complex.polar(math.sqrt(3), math.Pi/8)
    // a la WolframAlpha
    Complex.pow(Complex.xy(3, 4), Complex.xy(1, -2)) shouldEqual Complex.xy(-21.0831396906890219491181, -24.0002107094125670218826)
    Complex.pow(0, 3) shouldEqual 0
  }

  "Infinity" should "exist" in {
    Complex.Infinity shouldEqual Complex.Infinity
  }

  it should "be instantiatable" in {
    Complex.xy(Double.PositiveInfinity, 0) shouldEqual Complex.Infinity
    Complex.xy(Double.NegativeInfinity, 0) shouldEqual Complex.Infinity
    Complex.xy(0, Double.PositiveInfinity) shouldEqual Complex.Infinity
    Complex.xy(0, Double.NegativeInfinity) shouldEqual Complex.Infinity
    Complex.polar(Double.NegativeInfinity, math.Pi/4) shouldEqual Complex.Infinity
  }

  it should "support arithmetic operations" in {
    Complex.Infinity + Complex.xy(0, 0) shouldEqual Complex.Infinity
    Complex.Infinity - Complex.Infinity shouldEqual Complex.Infinity
    Complex.Infinity * Complex.Infinity shouldEqual Complex.Infinity
    Complex.xy(3, 7) / Complex.Infinity shouldEqual 0
    Complex.Infinity.recip() shouldEqual 0
    Complex.xy(0, 0).recip() shouldEqual Complex.Infinity
  }

  it should "support implicit casting" in {
    Complex.xy(0, 0) + Double.PositiveInfinity shouldEqual Complex.Infinity
  }

  // it should "provide trignometric functions" in {
  //   // TODO: sin and cos
  // }

}
