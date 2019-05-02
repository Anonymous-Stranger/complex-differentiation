package complexdiff
import complex._
import collection.TraversableOnce


case class ExprMap private(val m: Map[Expr, Complex]) extends Iterable[(Expr, Complex)] {

  def add(em: ExprMap): ExprMap =
    ExprMap((m.toIterator ++ em.m.toIterator))

  def add(e: Expr, c: Complex = 1): ExprMap = replace(e, get(e) + c)

  def replace(em: ExprMap): ExprMap = ExprMap(m ++ em.m)

  def replace(e: Expr, c: Complex): ExprMap = (m + (e -> c))

  def remove(e: Expr): ExprMap = (m - e)

  def get(e: Expr): Complex = m.get(e).getOrElse(0)

  def contains(e: Expr): Boolean = m.contains(e)

  def scale(c: Complex): TraversableOnce[(Expr, Complex)] =
    ExprMap(m.map { case (e, n) => e -> c*n })

  def iterator: Iterator[(Expr, Complex)] = m.iterator

}

object ExprMap {

  def apply() = new ExprMap(Map())

  def apply(items: TraversableOnce[(Expr, Complex)]): ExprMap
    = new ExprMap(items.toTraversable.groupBy(_._1).mapValues {
        groups => groups.map(_._2).fold(Complex.xy(0, 0))(_ + _)
      }
    )

  def apply[T](items: TraversableOnce[(Expr, T)])(implicit num: Numeric[T]): ExprMap
    = new ExprMap(items.toTraversable.groupBy(_._1).mapValues {
        groups => groups.map{
          case (e, n) => Complex.xy(num.toDouble(n), 0)
        }.fold(Complex.xy(0, 0))(_ + _)
      }
    )

  implicit def fromTraversable(items: TraversableOnce[(Expr, Complex)]): ExprMap =
    ExprMap(items)

}
