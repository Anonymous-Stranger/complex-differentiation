import complexdiff._

object Main {
	def main(args: Array[String]): Unit = {
		//val a = Z*Exp(Z.^(Const(2,0))+Const(5,0))
		val a = Sin(Exp(Z))*Cos(Z^Const(2))
		println(a)
		println(a.derivative())
		println((Power(Z, Const(3)) * Sin(Z)).whereEqual(Const(0)))
		println(Complex.xy(1, 2) + 7.3)

		println()
		// println((List(1, 2, 3) zip List(5, 6, 7)).filter(_ match {
		// 	case (a, b) => a != b
		// }))
		val e1 = Power(Z, Const(3)).whereEqual(Const(1))(0)
		println(e1)
		println(e1 == Z + Const(-1.0) * Exp(Quantifier() * Const(2.0943951023931953 * Complex.i)))
	}
}
