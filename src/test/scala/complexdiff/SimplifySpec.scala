package complexdiff

import org.scalatest._

class SimplifySpec extends FlatSpec with Matchers {

  "Expr" should "support equality comparison" in {
    Const(0) shouldEqual Const(0)
    Const(8) shouldNot equal (Const(3))
  }

  "constants" should "be evaluated" in {
    Power(Const(2), Const(3)).simplify() shouldEqual Const(8)
    Power(Power(Const(2), Const(2)), Const(2)).simplify() shouldEqual Const(16)
    Power(Z, Const(3)) shouldEqual Power(Z, Const(3.0))

    val a = Const(3 + 4*Complex.i)
    val alog = Const(Complex.xy(1.60943791243410037460075933, 0.92729521800161223242851246))
    Log(a).simplify shouldEqual alog
    Exp(alog).simplify shouldEqual a
    Exp(Log(a)).simplify shouldEqual a

    // TODO: sin and cos
  }

  it should "compress nested exponents" in {
    Power(Power(Z, Const(Complex.i*2)), Const(3)).simplify() shouldEqual Power(Z, Const(Complex.i*6))
  }

  "Sum" should "reduce empty sums" in {
    Sum(Nil).simplify() shouldEqual Const(0)
  }
  it should "reduce singleton sums" in {
    Sum(List(Z)).simplify() shouldEqual Z
    Sum(List(Const(7))).simplify() shouldEqual Const(7)
  }
  it should "drop zeroes" in {
    Sum(List(Const(0), Const(0), Z, Const(0), Z*Z)).simplify() shouldEqual Sum(List(Z, Z*Z))
  }

  "Product" should "reduce empty products" in {
    Product(Nil).simplify() shouldEqual Const(1)
  }
  it should "reduce singleton products" in {
    Product(List(Z)).simplify() shouldEqual Z
    Product(List(Const(7))).simplify() shouldEqual Const(7)
  }
  it should "drop ones" in {
    Product(List(Const(1), Const(1), Z, Const(1), Sin(Z))).simplify() shouldEqual Product(List(Z, Sin(Z)))
  }
  it should "detect zero" in {
    Product(List(Const(1), Const(0), Z, Const(1), Sin(Z))).simplify() shouldEqual Const(0)
  }

  "The whole" should "simplify 0 + 0 + z + 0" in {
    (Const(0) + Const(0) + Z + Const(0)).simplify() shouldEqual Z
  }
  it should "simplify 1 * z * e^{i2pi} + 0" in {
    (Const(1) * Z * Exp(Const(Complex.i*2*math.Pi)) + Const(0)).simplify() shouldEqual Z
  }



}
