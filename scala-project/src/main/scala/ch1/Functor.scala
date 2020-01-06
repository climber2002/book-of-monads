package ch1

trait Functor[F[_]] {
  def map[A, B](x: F[A])(f: A => B): F[B]
}