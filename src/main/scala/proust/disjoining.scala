package proust

object disjoining:

  sealed abstract class D[L,R]:
    def isLeft: Boolean
    def isRight: Boolean
    def identity: D[L,R] = this

  case class L[L](l: L) extends D[L,Nothing]:
    def isLeft: Boolean  = true
    def isRight: Boolean = false

  case class R[R](r: R) extends D[Nothing,R]:
    def isLeft: Boolean  = false
    def isRight: Boolean = true

  object option:
    enum Opt[+A]:
      def get: A =
        this match
          case Non()  => sys.error("Non.get")
          case The(a) => a

      def isEmpty: Boolean =
        this match
          case Non()  => true
          case The(a) => false

      def nonEmpty: Boolean =
        !isEmpty

      def getOrElse[A1 >: A](a: => A1): A1 =
        if nonEmpty then get else a

      val at: () => A =
        () => get

      case Non[Nothing]() extends Opt[Nothing]
      case The[A](a: A) extends Opt[A]

    object Opt:
      val non: Opt[Nothing] =
        Non()

      def empty[A]: Opt[A] =
        Non()

      def unit[A](a: A): Opt[A] =
        The(a)

      // scala library converstions

      def apply[A](a: A): Opt[A] =
        unit(a)

      def unapply[A](oa: Opt[A]): Option[A] =
        oa match
          case The(a) => Some(a)
          case Non()  => None