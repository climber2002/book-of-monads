object Exercise {
  /** Exercise 1.2 */
  def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case x :: xs => x :: concat(xs, ys)
  }

  /** Exercise 1.3 */
  def map2[A, B](f: A => B)(as: List[A]): List[B] = as.map(f)

  def singleton[A](a: A): List[A] = a :: Nil
}