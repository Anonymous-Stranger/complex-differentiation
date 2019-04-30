package complexdiff

import org.scalatest._
import complex._

class WhereEqualSpec extends FlatSpec with Matchers {

  "Const" should "evaluate to check equality" in {
    Const(0).whereEqual(Const(0)) should contain theSameElementsAs List(Const(0))
    Const(1).whereEqual(Const(0)) shouldBe empty
  }

  "Product" should "split when checking equal to 0" in {
    Product(List(Z, (Z + Const(2)), (Z - Const(5)))).whereZero() should contain theSameElementsAs (
      List(Z, (Z + Const(2)), (Z - Const(5)))
    )
    Product(List(Z, (Z + Const(2)), Const(5))).whereZero() should contain theSameElementsAs (
      List(Z, (Z + Const(2)))
    )
  }

  "Power" should "remove exponents" in {
    Power(Z, Const(3)).whereZero() should contain theSameElementsAs List(Z)
    Power(Z, Const(3)).whereEqual(Const(1)) should contain theSameElementsAs (
      List(Z - Exp(Quantifier() * Const(2*math.Pi/3*Complex.i)))
    )
  }

  "Exp" should "remove the e raised to" in {
    Exp(Z).whereZero() shouldBe empty
    Exp(Z).whereEqual(Const(1)) should contain theSameElementsAs (
      List(Z - Const(2*math.Pi*Complex.i)*Quantifier())
    )
    Exp(Const(1)/Z).whereZero() shouldBe empty
  }

  "Log" should "raise e to the value" in {
    Log(Z).whereZero() shouldBe List(Z + Const(-1))
    val c = 3 + 4*Complex.i
    Log(Z).whereEqual(Const(c)) shouldBe List(Z - Const(Complex.exp(c)))
  }



}
