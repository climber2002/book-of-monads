trait Container[C[_]] {
  def empty[A]: C[A]
  def insert[A](x: A, xs: C[A]): C[A]
}

object Container {
  /**Excercise 0.5
  */
  implicit val listContainer: Container[List] = new Container[List] {
    def empty[A]: List[A] = List.empty[A]

    def insert[A](x: A, xs: List[A]): List[A] = x :: xs
  }
}