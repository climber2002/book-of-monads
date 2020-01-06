package ch1

trait Monad[M[_]] {
  def point[A](x: A): M[A]

  def bind[A, B](x: M[A])(f: A => M[B]): M[B]
}

object Monad {
  implicit class MonadImplicit[M[_], A](ma: M[A]) {
    def flatMap[B](f: A => M[B])(implicit monad: Monad[M]): M[B] = monad.bind(ma)(f)

    def map[B](f: A => B)(implicit monad: Monad[M]): M[B] = monad.bind(ma)(a => monad.point(f(a)))
  }

  val optionMonad = new Monad[Option] {
    def point[A](x: A): Option[A] = Some(x)

    def bind[A, B](xo: Option[A])(f: A => Option[B]): Option[B] = xo match {
      case None => None
      case Some(x) => f(x)
    }
  }
}