package ch0

trait Eq[A] {
  def eq(x: A, y: A): Boolean
}