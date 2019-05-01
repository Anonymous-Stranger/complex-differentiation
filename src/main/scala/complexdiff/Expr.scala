package complexdiff
import language.implicitConversions
import math._
import complex._

case class ExprMap(m: Map[Expr, Complex]){
	def getOrElse(k: Expr, c: Complex): Complex = m.getOrElse(k,c)
	def add(k: Expr, c: Complex): ExprMap = ExprMap(m+(k -> (getOrElse(k,0)+c)))
	def add(m2: ExprMap): ExprMap = m2.m.foldLeft(this)((acc,kv)=>acc.add(kv._1,kv._2))
}

case object One extends Expr()(simplified = true) {
	/*override def toString(): String = "1"
	override def parenString(): String = "1"*/
}

case object Z extends Expr()(simplified = true) {
	/*override def toString(): String = "Z"
	override def parenString(): String = "Z"*/
}

case class Sum(es: ExprMap) extends Expr()()
case class Term(es: ExprMap) extends Expr()()
case class Exp(e1: Expr) extends Expr()()
case class Log(e1: Expr) extends Expr()()
case class Sin(e1: Expr) extends Expr()()
case class Cos(e1: Expr) extends Expr()()

// not for derivative
case class Quantifier() extends Expr()(simplified = true)
case class ASin(e1: Expr) extends Expr()()
case class ACos(e1: Expr) extends Expr()()
case class ATan(e1: Expr) extends Expr()()


class Expr()(var simplified: Boolean = false) {

	def +(e2: Expr): Expr = (this,e2) match {
		case (Sum(m1),Sum(m2)) => Sum(m1.add(m2))
		case (Sum(m1),_) => Sum(m1.add(e2,1))
		case (_,Sum(m2)) => Sum(m2.add(this,1))
		case (_,_) => Sum(ExprMap(Map(this->1,e2->1)))
	}
	def +(c: Complex): Expr = this+Sum(ExprMap(Map(One->c)))

	def -(e2:Expr) = this+(e2* -1)

	def *(e2: Expr) = (this,e2) match {
		case (Term(m1),Term(m2)) => Term(m1.add(m2))
		case (Term(m1),_) => Term(m1.add(e2,1))
		case (_,Term(m2)) => Term(m2.add(this,1))
		case (_,_) => Term(ExprMap(Map(this->1,e2->1)))
	}
	def *(c: Complex) = this match {
		case Sum(ExprMap(m1)) => Sum(ExprMap(m1.map((kv)=>(kv._1,kv._2*c))))
		case _ => Sum(ExprMap(Map(this->c)))
	}

	def /(e2:Expr) = this*(e2^ -1)

	def ^(c: Complex) = this match {
		case Term(ExprMap(m1)) => Term(ExprMap(m1.map((kv)=>(kv._1,kv._2*c))))
		case _ => Term(ExprMap(Map(this->c)))
	}

	def derivative(): Expr = this match {
		case Z => One
		case One => Expr.const(0)
		case Sum(ExprMap(m)) => {
			var sum = ExprMap(Map())
			m.foreach{ case (e,n) => sum=sum.add(e.derivative(),n) }
			return Sum(sum)
		}
		case Term(ExprMap(m)) => {
			val mult = Term(ExprMap(m.map(kv => (kv._1,kv._2-1))))
			var sum = ExprMap(Map())
			m.foreach{ case (e,n) => sum=sum.add(e.derivative()*Term(ExprMap(m-e)),n) }
			return Sum(sum)*mult
		}
		case Exp(e1) => e1.derivative()*Exp(e1)
		case Log(e1) => e1.derivative()*Term(ExprMap(Map(e1 -> -1)))
		case Sin(e1) => e1.derivative()*Cos(e1)
		case Cos(e1) => Expr.const(-1)*e1.derivative()*Sin(e1)
	}

	/*override def toString(): String = this match {
		case Z => "Z"
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
	}*/

	def whereZero(): List[Expr] = whereEqual(Expr.const(0))

	def whereEqual(e2: Expr): List[Expr] = Nil/*(this.simplify(), e2.simplify()) match {

		case (Product(es), const(c)) if c == 0 => es.flatMap(e => e.whereZero())

		case (Power(e1,const(c)), e2) => {e1.whereEqual(
			(Power(e2, const(1/c)) * Exp(Quantifier()*const(Complex.i*2*math.Pi/c)))
		)}

		case (Exp(e1), e2) => {
			e1.whereEqual((Log(e2) + const(Complex.i*2*math.Pi)*Quantifier()))
		}

		case (Log(e1), e2) => { e1.whereEqual(Exp(e2)) }

		case (Sin(e), e2) => { e.whereEqual(ASin(e2) + const(2*math.Pi)*Quantifier()) }

		case (Cos(e), e2) => { e.whereEqual(ACos(e2) + const(2*math.Pi)*Quantifier()) }

		// halting case
		case (e1, const(c)) if c == 0 => List(e1)
		// when all else fails, try simplifying the difference
		case (e1, e2) => (e1 - e2).simplify().whereZero()
	}*/

	def simplify(): Expr = if (simplified) this else simplifyStep().simplify()

	def simplifyStep(): Expr = this match {
		case e if e.simplified => e

		case Term(ExprMap(m)) if !m.forall({ case (e,n) => e.simplified}) => {
			var acc = ExprMap(Map())
			m.foreach{ case (e,n) => acc=acc.add(e.simplifyStep(),n) }
			return Term(acc)
		}
		case Term(ExprMap(m)) if m.exists({ case (e,n) => e==One||n==0 }) => Term(ExprMap(m.filterNot({ case (e,n) => e==One||n==0 })))
		case Term(ExprMap(m)) if m.size==1 && m.exists({ case (e,n) => n==1 }) => m.last._1
		case Term(ExprMap(m)) if m.size==0 || m.exists({ case (e,n) => e==Expr.const(0)||n==0 }) => Expr.const(0)

		case Sum(ExprMap(m)) if !m.forall({ case (e,n) => e.simplified}) => {
			var acc = ExprMap(Map())
			m.foreach{ case (e,n) => acc=acc.add(e.simplify(),n) }
			return Sum(acc)
		}
		case Sum(ExprMap(m)) if m.exists({ case (e,n) => n==0||e==Expr.const(0) }) => Term(ExprMap(m.filterNot({ case (e,n) => n==0||e==Expr.const(0) })))
		/*case Sum(Nil) => const(0)
		case Sum(e::Nil) => e
		case Sum(es) if !es.forall(_.simplified) => Sum(es.map(_.simplifyStep()))
		case Sum(es) if es.exists(_ == const(Complex.Infinity)) => const(Complex.Infinity)
		case Sum(es) if es.exists(_ == const(0)) => Sum(es.filter(_ != const(0)))
		case Sum(es) if es.exists(_ match { case Sum(_) => true ; case _ => fase }) =>


		case Product(Nil) => const(1)
		case Product(e::Nil) => e
		case Product(es) if es.exists(_ == const(0)) => const(0)
		case Product(es) if es.exists(_ == const(Complex.Infinity)) => const(Complex.Infinity)
		case Product(es) if !es.forall(_.simplified) => Product(es.map(_.simplifyStep()))
		case Product(es) if es.exists(_ == const(1)) => Product(es.filter(_ != const(1)))

		case Power(e, c) if !e.simplified => Power(e.simplifyStep(), c)
		case Power(const(a), const(b)) => const(Complex.pow(a, b))
		case Power(Power(e, const(b1)), const(b2)) => Power(e, const(b1 * b2))

		case Exp(e) if !e.simplified => Exp(e.simplifyStep())
		case Exp(const(c)) => const(Complex.exp(c))

		case Log(e) if !e.simplified => Log(e.simplifyStep())
		case Log(const(c)) => const(Complex.log(c))

		case Sin(e) if !e.simplified => Sin(e.simplifyStep())
		case Sin(const(c)) => const(Complex.sin(c))

		case Cos(e) if !e.simplified => Cos(e.simplifyStep())
		case Cos(const(c)) => const(Complex.cos(c))

		case ASin(e) if !e.simplified => ASin(e.simplifyStep())
		case ASin(const(c)) => const(Complex.asin(c))

		case ACos(e) if !e.simplified => ACos(e.simplifyStep())
		case ACos(const(c)) => const(Complex.acos(c))*/

		case e => { e.simplified = true ; e }
	}

	//def parenString(): String = "("+toString()+")"
}
object Expr {
	def const(n: Complex): Expr = if (n == 1) One else {
		val a = Sum(ExprMap(Map(One->n)))
		a.simplified = true
		return a
	}
}
