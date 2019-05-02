package complexdiff

import org.scalatest._
import complex._

class ExprMapSpec extends FlatSpec with Matchers {

  val items = List(Z -> Complex.xy(1, 0), One -> Complex.xy(0, 4))

  "Construction" should "accept traversables" in {
    "ExprMap()" should compile
    ExprMap(Map()) shouldEqual ExprMap()
    ExprMap(items) shouldEqual ExprMap(Map(items:_*))
    ExprMap(items.map(x => x)) shouldEqual ExprMap(items)
  }

  it should "combine duplicates" in {
    ExprMap(List((Z, Complex.xy(1, 0)), (Z, Complex.xy(0, 4)))) shouldEqual ExprMap(List(Z -> Complex.xy(1, 4)))
  }

  "ExprMaps" should "support getting coeffs" in {
    ExprMap(items).get(Z) shouldEqual 1
    ExprMap(items).get(One) shouldEqual 4*Complex.i
    ExprMap(items).get(Exp(Z)) shouldEqual 0
  }

  it should "support adding elements and traversables" in {
    ExprMap(items).add(Z, 2).get(Z) shouldEqual 3
    ExprMap(items).add(items) shouldEqual ExprMap(items ++ items)
  }

  it should "support replacing elements and traversables" in {
    ExprMap(items).replace(Z, 4).get(Z) shouldEqual 4
    ExprMap(items).replace(List()) shouldEqual ExprMap(items)
    ExprMap(items).replace(
      List(One -> Complex.xy(-2, 0), One -> Complex.xy(2, 3))
    ).get(One) shouldEqual Complex.xy(0, 3)
  }

  it should "support removing elements" in {
    ExprMap(items).remove(Z).contains(Z) shouldEqual false
  }

  it should "allow comparison with other ExprMaps" in {
    ExprMap(List(One -> Complex(1))) shouldEqual ExprMap(
      List(One -> Complex.polar(1, 0))
    )
  }

}
