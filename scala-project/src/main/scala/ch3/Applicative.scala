package ch3

import ch1.Monad
import ch1.Monad._
import ch1.Functor

trait Applicative[F[_]] extends Functor[F] {
  def point[A](x: A): F[A]

  def ap[A, B](x: F[A])(f: F[A => B]): F[B]

  /** Exercise 3.2 */
  def map[A, B](x: F[A])(f: A => B): F[B] = ap(x)(point(f))
}

object Applicative {
  /** Exercise 3.1 */
  def ap2[M[_], B, C](implicit m: Monad[M]): M[B => C] => M[B] => M[C] = 
    mf => mb => for {
      f <- mf
      b <- mb
    } yield f(b)
}