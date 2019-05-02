package complexdiff
import language.implicitConversions
import math._
import complex._


case object Z extends Expr()(simplified = true) {
	override def toString(): String = "z"
	override def parenString(): String = toString()
}
case object One extends Expr()(simplified = true) {
	override def isConst = true
	override def toConst = 1
}
case class LinExp(exprs: ExprMap) extends Expr()() {
	override def isConst = (exprs.size - (if (exprs.contains(One)) 1 else 0)) == 0
	override def toConst = exprs.get(One)
}
case class Term(exprs: ExprMap) extends Expr()() {
	override def isConst = (exprs.size - (if (exprs.contains(One)) 1 else 0)) == 0
	override def toConst = 1

}
case class Exp(e1: Expr) extends Expr()()
case class Log(e1: Expr) extends Expr()()
case class Sin(e1: Expr) extends Expr()()
case class Cos(e1: Expr) extends Expr()()


class Expr()(var simplified: Boolean = false) {

	def +(e2: Expr): Expr = Expr.sum(this, e2)
	def unary_-(): Expr = this*Expr.const(-1)
	def -(e2: Expr): Expr = this + -e2
	def *(e2: Expr): Expr = Expr.prod(this, e2)
	def recip(): Expr = Term(ExprMap(List(this -> -1)))
	def /(e2: Expr): Expr = this * e2.recip()

	def whereZero(): List[Expr] = whereEqual(Expr.const(0))

	def whereEqual(e2: Expr): List[Expr] = (this.simplify(), e2.simplify()) match {
		case (a, b) if a.isConst && b.isConst =>
			if (a.toConst == b.toConst) List(Expr.const(0)) else Nil

		// case (Term(es), b) if b.isConst =>
		// 	es.map { case (e, n)  }
	}

	def simplify(): Expr = if (simplified) this else simplifyStep().simplify()

	def simplifyStep(): Expr = this match {
		case e if e.simplified => e

		case LinExp(es) if es.exists(_._2 == 0) => LinExp(es.filter(_._2 != 0))
		case LinExp(es) if es.size == 1 && es.head._2 == 1 => es.head match {
			case (e, n) => e
		}
		case LinExp(es) if es.exists(!_._1.simplified) =>
			LinExp(es.map{case (e, n) => e.simplify -> n})
		case LinExp(es) if es.exists(Expr.isLinExp) => LinExp(es.flatMap{
			case (LinExp(es), n) => es.scale(n)
			case (e, n) => List(e -> n)
		})

		case Term(es) if es.exists(_._1 == 0) => Expr.const(0)
		case Term(es) if es.size == 1 && es.head._2 == 1 => es.head match {
			case (e, n) => e
		}
		case Term(es) if es.exists(!_._1.simplified) =>
			Term(es.map{case (e, n) => e.simplify -> n})
		case Term(es) if es.exists(Expr.isTerm) => Term(es.flatMap{
			case (Term(es), n) => es.scale(n)
			case (e, n) => List(e -> n)
		})
		case Term(es) if es.filter(_._1.isConst).size > 0 =>
			val (consts, terms) = es.partition(_._1.isConst)
			LinExp(List( Term(terms) -> consts.map{
				case (e, n) => Complex.pow(e.toConst(), n)
			}.fold(Complex(1))(_ * _) ))
		case Term(es) if es.isEmpty => One

		case Log(e) if !e.simplified => Log(e.simplifyStep())
		case Log(e) if e.isConst => Expr.const(Complex.log(e.toConst()))

		case Exp(e) if !e.simplified => Exp(e.simplifyStep())
		case Exp(e) if e.isConst => Expr.const(Complex.exp(e.toConst()))
		case Exp(LinExp(es)) if es.exists(Expr.isLog) => {
			val (terms, expterms) = es.partition(Expr.isLog)
			Expr.prod(List(Exp(LinExp(expterms))) ++ terms.flatMap{
				case (e, n) => List(Expr.pow(Exp(e), n))
			})
		}
		// We cannot simply cancel Log and Exp
		// However, Exp(Log(Exp)) can be cancelled, since Exp(Log(...))
		case Exp(Log(e)) => e
		case Exp(Term(es)) if es.exists(Expr.isLogExp) =>
				es.find(Expr.isLogExp) match {
			case Some((le@Log(Exp(e)), _)) => Exp(Term(es.remove(le).add(e)))
		}

		case Sin(e) if !e.simplified => Sin(e.simplifyStep())

		case Cos(e) if !e.simplified => Cos(e.simplifyStep())

		case e => { e.simplified = true ; e }
	}

	override def toString(): String = this match {
		case One => "1"
		case Z => "z"
		case LinExp(es) if es.size == 0 => Complex(0).toString
		case LinExp(es) => es.map{
			case (One, n) => n
			case (e, n) if n == 1 => e
			case (e, n) => s"($n)" ++ e.parenString
		}.mkString(" + ")
		case Term(es) => es.map{
			case (e, n) if n == 1 => e.parenString
			case (e, n) => e.parenString ++ s"^($n)"
		}.mkString("")
		case Exp(e) => "e^" + e.parenString
		case Log(e) => "log" + e.parenString
		case Sin(e) => "sin" + e.parenString
		case Cos(e) => "cos" + e.parenString
	}

	def isConst: Boolean = false
	def toConst(): Complex = 0

	def parenString(): String = "(" + toString() + ")"
}

object Expr {

	def sum(es: Expr*): Expr = sum(es)
	def sum(es: TraversableOnce[Expr]): Expr = LinExp(es.map(e => e -> Complex(1)))

	def prod(es: Expr*): Expr = prod(es)
	def prod(es: TraversableOnce[Expr]): Expr = Term(es.map(e => e -> Complex(1)))

	def pow(a: Expr, b: Expr): Expr =
		if (b.isConst) Term(List(a -> b.toConst())) else Exp(prod(b, Log(a)))

	private def isLinExp(kv: (Expr, Complex)): Boolean = kv match {
		case (LinExp(_), _) => true
		case _ => false
	}

	private def isTerm(kv: (Expr, Complex)): Boolean = kv match {
		case (Term(_), _) => true
		case _ => false
	}

	private def isLog(kv: (Expr, Complex)): Boolean = kv match {
		case (Log(_), _) => true
		case _ => false
	}

	private def isLogExp(kv: (Expr, Complex)): Boolean = kv match {
		case (Log(Exp(_)), n) if n == 1 => true
		case _ => false
	}

	implicit def const(z: Complex): Expr =
		if (z == 0) LinExp(Nil)
		else if (z == 1) One
		else LinExp(List(One -> z))

	implicit def const[T](c: T)(implicit num: Numeric[T]): Expr =
		const(Complex(num.toDouble(c)))
}
