package complexdiff.exprold
import language.implicitConversions
import math._
import complex._

case class Const(v: Complex) extends Expr(simplified = true) {
	override def parenString(): String = {
		if (v.re != 0 && v.im != 0) "(" + toString() + ")" else toString()
	}
}

case object Z extends Expr(simplified = true) {
	override def toString(): String = "Z"
	override def parenString(): String = "Z"
}

case class Sum(exprs: List[Expr]) extends Expr
case class Product(exprs: List[Expr]) extends Expr
case class Power(e1: Expr, c1: Const) extends Expr
case class Exp(e1: Expr) extends Expr
case class Log(e1: Expr) extends Expr
case class Sin(e1: Expr) extends Expr
case class Cos(e1: Expr) extends Expr

// not for derivative
case class Quantifier() extends Expr(simplified = true)
case class ASin(e1: Expr) extends Expr
case class ACos(e1: Expr) extends Expr
case class ATan(e1: Expr) extends Expr


class Expr(var simplified: Boolean = false) {

	def +(e2: Expr) = (this,e2) match {
		case (Sum(l1),Sum(l2)) => Sum(l1:::l2)
		case (Sum(l1),_) => Sum(e2::l1)
		case (_,Sum(l2)) => Sum(this::l2)
		case (Const(a), Const(b)) => Const(a + b)
		case (_,_) => Sum(List(this,e2))
	}

	def -(e2:Expr) = this+Product(List(Const(-1),e2))

	def *(e2: Expr) = (this,e2) match {
		case (Product(l1),Product(l2)) => Product(l1:::l2)
		case (Product(l1),_) => Product(e2::l1)
		case (_,Product(l2)) => Product(this::l2)
		case (_,_) => Product(List(this,e2))
	}

	def /(e2:Expr) = this*Power(e2,Const(-1))

	def ^(c: Const) = Power(this,c) // according to Scala's order of ops, this is after * and + for some reason

	def derivative(): Expr = this match {
		case Z => Const(1)
		case Const(_) => Const(0)
		case Sum(exprs) => Sum(exprs.map(e => e.derivative()))
		case Product(e1::exprs) => exprs match {
			case e2::Nil => Sum(List(Product(List(e1.derivative(),e2)),Product(List(e1,e2.derivative()))))
			case _ => Sum(Product(e1.derivative()::exprs)::(Product(exprs).derivative().asInstanceOf[Sum].exprs.map(e => e*e1)))
		}
		case Power(e1,Const(c)) => Product(List(e1.derivative(),Const(c),Power(e1,Const(c-1))))
		case Exp(e1) => Product(List(e1.derivative(),Exp(e1)))
		case Log(e1) => Product(List(e1.derivative(),Power(e1,Const(-1))))
		case Sin(e1) => Product(List(e1.derivative(),Cos(e1)))
		case Cos(e1) => Product(List(Const(-1),e1.derivative(),Sin(e1)))
	}

	override def toString(): String = this match {
		case Z => "Z"
		case Const(v) => v + ""
		case Sum(es) => es.mkString(" + ")
		case Product(es) => es.mkString(" * ")
		case Power(e1,c) => e1.parenString()+"^"+c.parenString()
		case Exp(e1) => "e^"+e1.parenString()
		case Log(e1) => "log"+e1.parenString()
		case Sin(e1) => "sin"+e1.parenString()
		case Cos(e1) => "cos"+e1.parenString()

		case Quantifier() => "n"
		case ASin(e1) => "asin"+e1.parenString()
		case ACos(e1) => "acos"+e1.parenString()
		case ATan(e1) => "atan"+e1.parenString()
	}

	def whereZero(): List[Expr] = whereEqual(Const(0))

	def whereEqual(e2: Expr): List[Expr] = (this.simplify(), e2.simplify()) match {
		case (Const(a), Const(b)) => if (a == b) List(Const(0)) else Nil

		case (Product(es), Const(c)) if c == 0 => es.flatMap(e => e.whereZero())

		case (Power(e1,Const(c)), e2) => {e1.whereEqual(
			(Power(e2, Const(1/c)) * Exp(Quantifier()*Const(Complex.i*2*math.Pi/c)))
		)}

		case (Exp(e1), e2) => {
			e1.whereEqual((Log(e2) + Const(Complex.i*2*math.Pi)*Quantifier()))
		}

		case (Log(e1), e2) => { e1.whereEqual(Exp(e2)) }

		case (Sin(e), e2) => { e.whereEqual(ASin(e2) + Const(2*math.Pi)*Quantifier()) }

		case (Cos(e), e2) => { e.whereEqual(ACos(e2) + Const(2*math.Pi)*Quantifier()) }

		// halting case
		case (e1, Const(c)) if c == 0 => List(e1)
		// when all else fails, try simplifying the difference
		case (e1, e2) => (e1 - e2).simplify().whereZero()
	}

	def simplify(): Expr = if (simplified) this else simplifyStep().simplify()

	def simplifyStep(): Expr = this match {
		case e if e.simplified => e

		case Sum(Nil) => Const(0)
		case Sum(e::Nil) => e
		case Sum(es) if !es.forall(_.simplified) => Sum(es.map(_.simplifyStep()))
		case Sum(es) if es.exists(_ == Const(Complex.Infinity)) => Const(Complex.Infinity)
		case Sum(es) if es.exists(_ == Const(0)) => Sum(es.filter(_ != Const(0)))

		case Product(Nil) => Const(1)
		case Product(e::Nil) => e
		case Product(es) if es.exists(_ == Const(0)) => Const(0)
		case Product(es) if es.exists(_ == Const(Complex.Infinity)) => Const(Complex.Infinity)
		case Product(es) if !es.forall(_.simplified) => Product(es.map(_.simplifyStep()))
		case Product(es) if es.exists(_ == Const(1)) => Product(es.filter(_ != Const(1)))

		case Power(e, c) if !e.simplified => Power(e.simplifyStep(), c)
		case Power(Const(a), Const(b)) => Const(Complex.pow(a, b))
		case Power(Power(e, Const(b1)), Const(b2)) => Power(e, Const(b1 * b2))

		case Exp(e) if !e.simplified => Exp(e.simplifyStep())
		case Exp(Const(c)) => Const(Complex.exp(c))

		case Log(e) if !e.simplified => Log(e.simplifyStep())
		case Log(Const(c)) => Const(Complex.log(c))

		case Sin(e) if !e.simplified => Sin(e.simplifyStep())
		case Sin(Const(c)) => Const(Complex.sin(c))

		case Cos(e) if !e.simplified => Cos(e.simplifyStep())
		case Cos(Const(c)) => Const(Complex.cos(c))

		case ASin(e) if !e.simplified => ASin(e.simplifyStep())
		case ASin(Const(c)) => Const(Complex.asin(c))

		case ACos(e) if !e.simplified => ACos(e.simplifyStep())
		case ACos(Const(c)) => Const(Complex.acos(c))

		case e => { e.simplified = true ; e }
	}

	def parenString(): String = "("+toString()+")"
}
