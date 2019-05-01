package complexdiff
import language.implicitConversions
import math._
import complex._

case class ExprMap(m: Map[Expr, Complex]){
	def getOrElse(k: Expr, c: Complex): Complex = m.getOrElse(k,c)
	def add(k: Expr, c: Complex): ExprMap = ExprMap(m+(k -> (getOrElse(k,0)+c)))
	def add(m2: ExprMap): ExprMap = m2.m.foldLeft(this)((acc,kv)=>acc.add(kv._1,kv._2))
	def mult(c: Complex): ExprMap = ExprMap(m.map((kv => (kv._1,kv._2*c))))
}

case object One extends Expr()(simplified = true)

case object Z extends Expr()(simplified = true)

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
		case Sum(em) => Sum(em.mult(c))
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

	override def toString(): String = this match {
		case Z => "Z"
		case One => "1"
		case Sum(ExprMap(m)) => m.foldLeft("")((acc,kv) => acc+(if(acc.size==0) "" else "+")+kv._1.timesString(kv._2))
		case Term(ExprMap(m)) => m.foldLeft("")((acc,kv) => acc+(if(acc.size==0) "" else "*")+kv._1.powerString(kv._2))
		case Exp(e1) => "e^"+e1.parenString()
		case Log(e1) => "log("+e1.toString()+")"
		case Sin(e1) => "sin("+e1.toString()+")"
		case Cos(e1) => "cos("+e1.toString()+")"

		case Quantifier() => "n"
		case ASin(e1) => "asin("+e1.toString()+")"
		case ACos(e1) => "acos("+e1.toString()+")"
		case ATan(e1) => "atan("+e1.toString()+")"
	}

	def parenString(): String = this match {
		case Z | One | Quantifier() | Sin(_) | Cos(_) | Log(_) | ASin(_) | ACos(_) | ATan(_) => toString()
		case _ => "("+toString()+")"
	}

	def timesString(c: Complex): String = this match {
		case One => c.toString()
		case _ => if(c!=1) c.toString()+"*"+parenString() else parenString()
	}

	def powerString(c: Complex): String = if(c==1) parenString() else parenString()+"^"+c.toString()


	def whereZero(): List[Expr] = whereEqual(Expr.const(0))

	def whereEqual(e2: Expr): List[Expr] = (this.simplify(), e2.simplify()) match {

		case (Term(ExprMap(m)), c) if Expr.isConstVal(c,0) => m.flatMap(kv => kv._1.whereZero()).toList

		case (Term(ExprMap(m)), e2) if m.size==1 => {m.last._1.whereEqual(
			(Term(ExprMap(Map(e2 -> (m.last._2.recip())))) * Exp(Quantifier()*Expr.const(Complex.i*2*math.Pi/m.last._2)))
		)}

		case (Exp(e1), e2) => {
			e1.whereEqual((Log(e2) + Expr.const(Complex.i*2*math.Pi)*Quantifier()))
		}

		case (Log(e1), e2) => { e1.whereEqual(Exp(e2)) }

		case (Sin(e), e2) => { e.whereEqual(ASin(e2) + Expr.const(2*math.Pi)*Quantifier()) }

		case (Cos(e), e2) => { e.whereEqual(ACos(e2) + Expr.const(2*math.Pi)*Quantifier()) }

		// halting case
		case (e1, c) if Expr.isConstVal(c,0) => List(e1)
		// when all else fails, try simplifying the difference
		case (e1, e2) => (e1 - e2).simplify().whereZero()
	}

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
		case Term(ExprMap(m)) if m.exists(kv => Expr.hasConst(kv._1)) => {
			var term = ExprMap(m.filterNot(kv => Expr.hasConst(kv._1)))
			var c: Complex = 1
			m.foreach{ case (e,n) => {
				if(Expr.hasConst(e)){
					c = c*Complex.pow(Expr.getConst(e),n)
					if(!Expr.isConst(e)) term = term.add(e.asInstanceOf[Sum].es.m.last._1,n)
				}
			}}
			return Sum(ExprMap(Map(Term(term)->c)))
		}
		case Term(ExprMap(m)) if m.exists(kv => kv._1 match { case Term(_) => true ; case _ => false }) => {
			var acc = ExprMap(Map())
			m.foreach{ case(e,n) => e match {
				case Term(em) => acc = acc.add(em.mult(n))
				case _ => acc = acc.add(e,n)
			}}
			return Term(acc)
		}

		case Sum(ExprMap(m)) if !m.forall({ case (e,n) => e.simplified}) => {
			var acc = ExprMap(Map())
			m.foreach{ case (e,n) => acc=acc.add(e.simplify(),n) }
			return Sum(acc)
		}
		case Sum(ExprMap(m)) if m.exists({ case (e,n) => n==0||e==Expr.const(0) }) => Term(ExprMap(m.filterNot({ case (e,n) => n==0||e==Expr.const(0) })))
		//case Sum(Nil) => const(0)
		//case Sum(e::Nil) => e
		//case Sum(es) if es.exists(_ == const(Complex.Infinity)) => const(Complex.Infinity)
		case Sum(ExprMap(m)) if m.exists(kv => kv._1 match { case Sum(_) => true ; case _ => false }) => {
			var acc = ExprMap(Map())
			m.foreach{ case(e,n) => e match {
				case Sum(em) => acc = acc.add(em.mult(n))
				case _ => acc = acc.add(e,n)
			}}
			return Sum(acc)
		}


		//case Product(Nil) => const(1)
		//case Product(e::Nil) => e
		//case Product(es) if es.exists(_ == const(Complex.Infinity)) => const(Complex.Infinity)

		case Exp(e) if !e.simplified => Exp(e.simplifyStep())
		case Exp(e) if Expr.isConst(e) => Expr.const(Complex.exp(Expr.getConst(e)))

		case Log(e) if !e.simplified => Log(e.simplifyStep())
		case Log(e) if Expr.isConst(e) => Expr.const(Complex.log(Expr.getConst(e)))

		case Sin(e) if !e.simplified => Sin(e.simplifyStep())
		case Sin(e) if Expr.isConst(e) => Expr.const(Complex.sin(Expr.getConst(e)))

		case Cos(e) if !e.simplified => Cos(e.simplifyStep())
		case Cos(e) if Expr.isConst(e) => Expr.const(Complex.cos(Expr.getConst(e)))

		case ASin(e) if !e.simplified => ASin(e.simplifyStep())
		case ASin(e) if Expr.isConst(e) => Expr.const(Complex.asin(Expr.getConst(e)))

		case ACos(e) if !e.simplified => ACos(e.simplifyStep())
		case ACos(e) if Expr.isConst(e) => Expr.const(Complex.acos(Expr.getConst(e)))

		case e => { e.simplified = true ; e }
	}
}
object Expr {
	def const(n: Complex): Expr = if (n == 1) One else {
		val a = Sum(ExprMap(Map(One->n)))
		a.simplified = true
		return a
	}
	def hasConst(e: Expr): Boolean = e match { // includes sums with a single term
		case Sum(ExprMap(m)) if m.size==1 => true
		case _ => Expr.isConst(e)
	}
	def isConst(e: Expr): Boolean = e match {
		case Sum(ExprMap(m)) if m.size==1 && m.last._1==One => true
		case One => true
		case _ => false
	}
	def isConstVal(e: Expr, c: Complex): Boolean = isConst(e)&&getConst(e)==c
	def getConst(e: Expr): Complex = e match {
		case Sum(ExprMap(m)) if m.size==1 => m.last._2
		case One => 1
		case _ => null
	}
}
