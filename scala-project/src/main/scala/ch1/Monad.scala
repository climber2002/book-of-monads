trait Monad[M[_]] {
  def point[A](x: A): M[A]

  def bind[A, B](x: M[A])(f: A => M[B]): M[B]
}

object Monad {
  val optionMonad = new Monad[Option] {
    def point[A](x: A): Option[A] = Some(x)

    def bind[A, B](xo: Option[A])(f: A => Option[B]): Option[B] = xo match {
      case None => None
      case Some(x) => f(x)
    }
  }
}