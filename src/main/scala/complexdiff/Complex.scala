package complexdiff
import language.implicitConversions
import math._


trait Complex {
  def re: Double
  def im: Double
  def mag: Double
  def arg: Double
  override def equals(v: Any) = v match {
    case c: Complex => equivalentTo(c)
    case v: java.lang.Number => equivalentTo(Complex.fromNumeric(v.doubleValue()))
    case _ => false
  }
  def equivalentTo(c: Complex): Boolean = {
    val err = 0.0000000000001
    // val err = math.max((mag + c.mag)*smallError, smallError)
    math.abs(re - c.re) < err && math.abs(im - c.im) < err
  }
  def +(c: Complex): Complex
  def -(c: Complex): Complex = this + -c
  def unary_-(): Complex = Complex.fromNumeric(0) - this
  def *(c: Complex): Complex
  def recip(): Complex = Complex.fromNumeric(1) / this
  def /(c: Complex): Complex = this * c.recip()
  override def toString() = {
    if (im == 0) "" + re
		else if (re == 0) im + "i"
		else s"$re+${im}i"
  }
}


object Complex {

  def i : Complex = xy(0, 1)

  private class _C(val _re: Double, val _im: Double) extends Complex {
    def re = _re
    def im = _im
    def mag = { math.sqrt(re*re + im*im) }
    def arg = { math.atan2(im, re) }
    def +(c: Complex) = Complex.xy(re + c.re, im + c.im)
    override def unary_-(): Complex = Complex.xy(-re, -im)
    def *(c: Complex) = Complex.xy(re * c.re - im * c.im, re*c.im + im*c.re)
    override def recip() = Complex.polar(1/mag, -arg)
  }

  def xy(re: Double, im: Double): Complex = new _C(re, im)
  def polar(r: Double, theta: Double): Complex = {
    new _C(r*math.cos(theta), r*math.sin(theta))
  }

  def log(x: Complex): Complex = Complex.xy(math.log(x.mag), x.arg)

  def exp(x: Complex): Complex = Complex.polar(math.exp(x.re), x.im)

  def pow(a: Complex, b: Complex): Complex =
    if (a != 0 || b == 0) Complex.exp(b * Complex.log(a)) else 0


  implicit def fromNumeric[T](re: T)(implicit num: Numeric[T]): Complex = {
    Complex.xy(num.toDouble(re), 0)
  }

}
