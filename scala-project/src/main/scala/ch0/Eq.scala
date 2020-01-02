package ch0

trait Eq[A] {
  def eq(x: A, y: A): Boolean
}

object Eq {
  def eqListImpl[A](xs: List[A], ys: List[A])(implicit eq: Eq[A]): Boolean = (xs, ys) match {
    case (Nil, Nil) => true
    case (x :: xs1, y :: ys1) => eq.eq(x, y) && eqListImpl(xs1, ys1)
    case _ => false
  }

  implicit def eqList[A](implicit eqElement: Eq[A]): Eq[List[A]] = (xs: List[A], ys: List[A]) => eqListImpl(xs, ys)(eqElement)

  implicit val eqBoolean: Eq[Boolean] = (x: Boolean, y: Boolean) => (x, y) match {
    case (true, true) => true
    case (false, false) => true
    case _ => false
  }

  implicit def eqTuple[A, B](implicit eqA: Eq[A], eqB: Eq[B]): Eq[(A, B)] = (t1: (A, B), t2: (A, B)) => eqA.eq(t1._1, t2._1) && eqB.eq(t1._2, t2._2)
  
}