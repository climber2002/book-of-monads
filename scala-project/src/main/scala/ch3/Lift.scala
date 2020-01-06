package ch3

import ch1.Monad
import ch1.Monad._

object Lift {
  def lift2[M[_], A, B, C](f: (A, B) => C)(implicit m: Monad[M]): (M[A], M[B]) => M[C] = 
    (x, y) => for { a <- x; b <- y } yield f(a, b)
}

