import complexdiff._
import complex._

object Main {
	def main(args: Array[String]): Unit = {
		val e = Cos(Z)+(Sin(Z)*2)*(Exp(Z)*2)
		println(e.simplify())
		/*val x = Const(1)+Z
		val y = Const(1)+Z+Const(0)
		println(x==y)
		println(x==y.simplify())
		val asdf: Map[Expr,Complex] = Map(x->Complex.i)
		val m = asdf+(y.simplify() -> Complex.xy(4,5))
		println(m.get(y).getOrElse(0))
		println(m.get(y.simplify()).getOrElse(0))*/
		/*//val a = Z*Exp(Z.^(Const(2,0))+Const(5,0))
		val a = Sin(Exp(Z))*Cos(Z^Const(2))
		println(a)
		println(a.derivative())
		println((Power(Z, Const(3)) * Sin(Z)).whereEqual(Const(0)))
		println(Complex.xy(1, 2) + 7.3)

		println(Complex.Infinity == Double.PositiveInfinity)
		// println((List(1, 2, 3) zip List(5, 6, 7)).filter(_ match {
		// 	case (a, b) => a != b
		// }))
		val e1 = Power(Z, Const(3)).whereEqual(Const(1))(0)
		println(e1)
		println(e1 == Z + Const(-1.0) * Exp(Quantifier() * Const(2.0943951023931953 * Complex.i)))*/
	}
}
