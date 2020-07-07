package proust

sealed abstract class D[L,R] {
  def isLeft: Boolean
  def isRight: Boolean

  def identity: D[L,R] = this

}

case class L[L](l: L) extends D[L,Nothing] {
  def isLeft: Boolean  = true
  def isRight: Boolean = false
}

case class R[R](r: R) extends D[Nothing,R] {
  def isLeft: Boolean  = true
  def isRight: Boolean = false
}

sealed trait O[A] {
  def isEmpty: Boolean
  def notEmpty: Boolean = !isEmpty
}

object O {
  def N[A]: O[A] = new N[A]()
  def S[A](a: A): O[A] = new S(a)
}

private case class S[A](a: A) extends O[A] {
  def isEmpty: Boolean = false
}

private final class N[A]() extends O[A] {
  def isEmpty: Boolean = true
}