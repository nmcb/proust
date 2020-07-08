package proust

object disjoining {

sealed abstract class D[L,R] {

  def isLeft: Boolean

  def isRight: Boolean

  def identity: D[L,R] =
    this
}

case class L[L](l: L) extends D[L,Nothing] {

  def isLeft: Boolean  =
    true

  def isRight: Boolean =
    false
}

case class R[R](r: R) extends D[Nothing,R] {

  def isLeft: Boolean  =
    true
  
  def isRight: Boolean =
    false
}

object option {

  sealed trait O[A] {

    def get: A

    def isEmpty: Boolean

    def nonEmpty: Boolean =
      !isEmpty

    def getOrElse(a: => A): A =
      if (nonEmpty) then get else a
  }

  def N[A]: O[A] =
    new O[A] {

    def get =
      sys.error("N.get")

    def isEmpty: Boolean =
      true
  }

  def S[A](a: => A) : O[A] =
    new O[A] {

      val at: () => A =
        () => a

      def get: A =
        at()

      def isEmpty: Boolean =
        false
  }
}
}