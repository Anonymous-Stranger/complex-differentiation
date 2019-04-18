object Main {
	def main(args: Array[String]): Unit = {
		val a = Z*Exp(Z.^(Const(2,0))+Const(5,0))
		println(a)
		println(a.derivative())
	}
}
