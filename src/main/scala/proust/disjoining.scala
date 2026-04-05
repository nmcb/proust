package proust

object disjoining:

  enum D[L, R]:
    def isLeft: Boolean =
      this match
        case D.L(l) => true
        case D.R(r) => false
    def isRight: Boolean =
      this match
        case D.L(l) => false
        case D.R(r) => true
    def identity: D[L, R] =
      this

    private case L(l: L) extends D[L, Nothing]
    private case R(r: R) extends D[Nothing, R]

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

      private def nonEmpty: Boolean =
        !isEmpty

      def getOrElse[B >: A](a: => B): B =
        if nonEmpty then get else a

      val at: () => A =
        () => get

      private case Non()     extends Opt[Nothing]
      private case The(a: A) extends Opt[A]

    object Opt:
      
      val non: Opt[Nothing] =
        Non()

      def empty[A]: Opt[A] =
        Non()

      def unit[A](a: A): Opt[A] =
        The(a)

      // scala library conversions

      def apply[A](a: A): Opt[A] =
        unit(a)

      def unapply[A](oa: Opt[A]): Option[A] =
        oa match
          case The(a) => Some(a)
          case Non()  => None
