case class Const(re: Double, im: Double) extends Expr {
	//TODO: make implicit constructor
	override def toString(): String = {
		if(im==0) ""+re
		else if(re==0) im+"i"
		else s"$re+${im}i"
	}
	override def parenString(): String = if(im!=0&&re!=0) "("+toString()+")" else toString()
	def +(c: Const): Const = Const(re+c.re,im+c.im) // We still need to simplify, in case someone explicitly creates Sums/Products,etc
	def -(c: Const): Const = Const(re-c.re,im-c.im)
	def *(c: Const): Const = Const(re*c.re-im*c.im,re*c.im+im*c.re)
	def /(c: Const): Const = {
		val m = c.re*c.re+c.im*c.im
		Const((re*c.re+im*c.im)/m,(im*c.re-re*c.im)/m)
	}
}
case object Z extends Expr {
	override def toString(): String = "Z"
	override def parenString(): String = "Z"
}
case class Sum(exprs: List[Expr]) extends Expr
case class Product(exprs: List[Expr]) extends Expr
case class Power(e1: Expr, c1: Const) extends Expr
case class Exp(e1: Expr) extends Expr

class Expr(){
	def +(e2: Expr) = (this,e2) match {
		case (Sum(l1),Sum(l2)) => Sum(l1:::l2)
		case (Sum(l1),_) => Sum(e2::l1)
		case (_,Sum(l2)) => Sum(this::l2)
		case (_,_) => Sum(List(this,e2))
	}
	def -(e2:Expr) = this+Product(List(Const(-1,0),e2))
	def *(e2: Expr) = (this,e2) match {
		case (Product(l1),Product(l2)) => Product(l1:::l2)
		case (Product(l1),_) => Product(e2::l1)
		case (_,Product(l2)) => Product(this::l2)
		case (_,_) => Product(List(this,e2))
	}
	def /(e2:Expr) = this*Power(e2,Const(-1,0))
	def ^(c: Const) = Power(this,c) // according to Scala's order of ops, this is after * and + for some reason
	def derivative(): Expr = this match {
		case Z => Const(1,0)
		case Const(_,_) => Const(0,0)
		case Sum(exprs) => Sum(exprs.map(e => e.derivative()))
		case Product(e1::exprs) => exprs match {
			case e2::Nil => Sum(List(Product(List(e1.derivative(),e2)),Product(List(e1,e2.derivative()))))
			case _ => Sum(Product(e1.derivative()::exprs)::(Product(exprs).derivative().asInstanceOf[Sum].exprs.map(e => e*e1)))
		}
		case Power(e1,c) => Product(List(e1.derivative(),c,Power(e1,c-Const(1,0))))
		case Exp(e1) => Product(List(e1.derivative(),Exp(e1)))
	}
	override def toString(): String = this match {
		case Z => "Z"
		case Const(re,im) => s"$re+${im}i"
		case Sum(first::rest) => first.parenString()+(if(rest==Nil) "" else ("+" +Sum(rest).toString()))
		case Product(first::rest) => first.parenString()+(if(rest==Nil) "" else ("*" +Product(rest).toString()))
		case Power(e1,c) => e1.parenString()+"^"+c.parenString()
		case Exp(e1) => "e^"+e1.parenString()
	}
	def parenString(): String = "("+toString()+")"
}
