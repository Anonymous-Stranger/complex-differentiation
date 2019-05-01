package complex
import language.implicitConversions
import math._


trait Complex {
  def re: Double
  def im: Double
  def mag: Double
  def arg: Double
  override def equals(v: Any): Boolean = v match {
    case c: Complex => equivalentTo(c)
    case v: java.lang.Number => equivalentTo(Complex.fromNumeric(v.doubleValue()))
    case _ => false
  }
  def equivalentTo(c: Complex): Boolean = {
    val err = 0.0000000000001
    // val err = math.max((mag + c.mag)*smallError, smallError)
    math.abs(re - c.re) < err && math.abs(im - c.im) < err
  }
  def +(c: Complex): Complex = Complex.add(this, c)
  def -(c: Complex): Complex = Complex.sub(this, c)
  def unary_-(): Complex = Complex.neg(this)
  def *(c: Complex): Complex = Complex.mult(this, c)
  def recip(): Complex = Complex.recip(this)
  def /(c: Complex): Complex = Complex.div(this, c)
  override def toString() = {
    if (im == 0) "" + re
		else if (re == 0) im + "i"
		else s"($re+${im}i)"
  }
}


object Complex {

  def i : Complex = xy(0, 1)

  object Infinity extends Complex {
    def re = Double.PositiveInfinity
    def im = Double.PositiveInfinity
    def mag = Double.PositiveInfinity
    def arg = 0

    override def equals(v: Any): Boolean = v match {
      case v: Complex => v.mag == mag
      case v: java.lang.Number => math.abs(v.doubleValue()) == mag
      case _ => false
    }

    override def toString(): String = "Infinity"
  }

  private class _C(val _re: Double, val _im: Double) extends Complex {
    def re = _re
    def im = _im
    def mag = { math.sqrt(re*re + im*im) }
    def arg = { math.atan2(im, re) }
  }

  def xy(re: Double, im: Double): Complex = {
    if (abs(re) + abs(im) == Double.PositiveInfinity) Infinity
    else new _C(re, im)
  }
  def polar(r: Double, theta: Double): Complex = {
    if (abs(r) == Double.PositiveInfinity) Infinity
    else new _C(r*math.cos(theta), r*math.sin(theta))
  }

  def log(x: Complex): Complex = Complex.xy(math.log(x.mag), x.arg)

  def exp(x: Complex): Complex = Complex.polar(math.exp(x.re), x.im)

  def pow(a: Complex, b: Complex): Complex =
    if (a != 0 || b == 0) Complex.exp(b * Complex.log(a)) else 0

  def sqrt(x: Complex): Complex = Complex.pow(x, 0.5)

  def sinh(x: Complex): Complex = (exp(x) - exp(-x)) / 2
  def cosh(x: Complex): Complex = (exp(x) + exp(-x)) / 2

  def sin(x: Complex): Complex = sinh(Complex.i * x) / i
  def cos(x: Complex): Complex = cosh(Complex.i * x)
  def tan(x: Complex): Complex = sin(x) / cos(x)

  // http://scipp.ucsc.edu/~haber/archives/physics116A10/arc_10.pdf
  def asin(x: Complex): Complex = Complex.log(i*x + Complex.sqrt(1 - x*x)) / i
  def acos(x: Complex): Complex = math.Pi/2 - asin(x)
  def atan(x: Complex): Complex = Complex.log((i - x)/(i + x)) / (2*i)


  implicit def fromNumeric[T](re: T)(implicit num: Numeric[T]): Complex = {
    Complex.xy(num.toDouble(re), 0)
  }

  private[complex] def add(a: Complex, b: Complex): Complex = (a, b) match {
    case (a: _C, b: _C) => Complex.xy(a.re + b.re, a.im + b.im)
    case _ => Infinity
  }

  private[complex] def sub(a: Complex, b: Complex): Complex = a + -b

  private[complex] def neg(x: Complex): Complex = x match {
    case x: _C => Complex.xy(-x.re, -x.im)
    case Infinity => Infinity
  }

  private[complex] def mult(a: Complex, b: Complex): Complex = (a, b) match {
    case (a, _) if a == 0 => 0
    case (_, b) if b == 0 => 0
    case (a: _C, b: _C) =>
      Complex.xy(a.re * b.re - a.im * b.im, a.re*b.im + a.im*b.re)
    case _ => Infinity
  }

  private[complex] def recip(x: Complex): Complex = x match {
    case x: _C => Complex.polar(1/x.mag, -x.arg)
    case Infinity => 0
  }

  private[complex] def div(a: Complex, b: Complex): Complex = a * b.recip()

}
