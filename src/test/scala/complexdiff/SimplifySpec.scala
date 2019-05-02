package complexdiff

import org.scalatest._
import complex._

class SimplifySpec extends FlatSpec with Matchers {

  "Expr" should "support equality comparison" in {
    Expr.const(0) shouldEqual Expr.const(0)
    Expr.const(7) shouldEqual Expr.const(3 + 4)
    Expr.const(8) shouldNot equal (3)
  }

  "constants" should "be detected" in {
    Expr.const(7).isConst shouldBe true
    Sin(Z).isConst shouldBe false
    Sin(Expr.const(3)).isConst shouldBe false
    Expr.const(4).toConst shouldEqual 4
    Expr.const(Complex.xy(5, 7)).toConst shouldEqual Complex.xy(5, 7)
  }

  "constants" should "be evaluated" in {
    val a = Expr.const(3 + 4*Complex.i)
    val alog = Expr.const(Complex.xy(1.60943791243410037460075933, 0.92729521800161223242851246))
    Log(a).simplify shouldEqual alog
    Exp(alog).simplify shouldEqual a
    Exp(Log(a)).simplify shouldEqual a

    Expr.pow(Expr.const(2), Expr.const(3)).simplify() shouldEqual Expr.const(8)
    Expr.pow(Expr.pow(Expr.const(2), Expr.const(2)), Expr.const(2)).simplify() shouldEqual Expr.const(16)
    Expr.pow(Z, Expr.const(3)) shouldEqual Expr.pow(Z, Expr.const(3.0))

    // TODO: sin and cos
  }
  //
  it should "compress nested exponents" in {
    Expr.pow(Expr.pow(Z, Expr.const(Complex.i*2)), Expr.const(3)).simplify() shouldEqual Term(List(Z->6*Complex.i))
  }

  "LinExp" should "reduce empty sums" in {
    LinExp(ExprMap(List(Expr.const(3) -> 0))).simplify() shouldEqual Expr.const(0).simplify()
    Expr.const(0).simplify().toString shouldEqual "0.0"
  }
  it should "reduce singleton sums" in {
    Expr.sum(List(Z)).simplify() shouldEqual Z
    Expr.sum(List(Expr.const(7))).simplify() shouldEqual Expr.const(7)
  }
  it should "drop zeroes" in {
    Expr.sum(List(Expr.const(0), Expr.const(0), Z, Expr.const(0), Z*Z)).simplify() shouldEqual Expr.sum(List(Z, Z*Z)).simplify()
  }

  "Term" should "reduce empty products" in {
    Expr.prod(Nil).simplify() shouldEqual Expr.const(1)
  }
  it should "reduce singleton products" in {
    Expr.prod(List(Z)).simplify() shouldEqual Z
    Expr.prod(List(Expr.const(7))).simplify() shouldEqual Expr.const(7)
  }
  it should "drop ones" in {
    Expr.prod(List(Expr.const(1), Expr.const(1), Z, Expr.const(1), Sin(Z))).simplify() shouldEqual Expr.prod(List(Z, Sin(Z)))
  }
  it should "detect zero" in {
    Expr.prod(List(Expr.const(1), Expr.const(0), Z, Expr.const(1), Sin(Z))).simplify() shouldEqual Expr.const(0)
  }

  "Log" should "not invert Exp" in {
    Log(Exp(Z)).simplify shouldBe Log(Exp(Z))
  }

  "The whole" should "simplify 0 + 0 + z + 0" in {
    (Expr.const(0) + Expr.const(0) + Z + Expr.const(0)).simplify() shouldEqual Z
  }
  it should "simplify 1 * z * e^{i2pi} + 0" in {
    (Expr.const(1) * Z * Exp(Expr.const(Complex.i*2*math.Pi)) + Expr.const(0)).simplify() shouldEqual Z
  }
  // TODO: support factoring.
  // it should "simplify z^z * z^{-z}" in {
  //   (Expr.pow(Z, Z) * Expr.pow(Z, -Z)).simplify() shouldEqual Expr.const(1)
  // }



}
