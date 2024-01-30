package proust

object sequencing:

  import disjoining._
  import option._

  enum Seq[+A]:

    def a: A =
      this match
        case End()    => sys.error("No head on Nil")
        case Cel(h,_) => h

    def opt: Opt[A] =
      this match
        case End()    => Opt.non
        case Cel(a,r) => Opt(a)

    def r: Seq[A] =
      this match
        case Cel(_,r) => r
        case End()    => sys.error("No head on Nil")

    def ::[A1 >: A](a: A1): Seq[A1] =
      Cel(a,this)

    def ++[A1 >: A](rhs: Seq[A1]): Seq[A1] =
      this match
        case Cel(a,t) => Cel(a, t ++ rhs)
        case End()    => rhs

    def map[B](f: A => B): Seq[B] =
      this match
        case End()    => Seq.empty
        case Cel(a,r) => Cel(f(a),r.map(f))

    def bind[B](f: A => Seq[B]): Seq[B] =
      this match
        case End()    => Seq.empty
        case Cel(a,r) => f(a) ++ r.bind(f)

    def flatMap[B](f: A => Seq[B]): Seq[B] =
      bind(f)

    def foldl[B](b: B)(f: B => A => B): B =
      this match
        case End()    => b
        case Cel(a,r) => r.foldl(f(b)(a))(f)

    def foldr[B](b: B)(f: A => B => B): B =
      this match
        case End()    => b
        case Cel(a,r) => f(a)(r.foldr(b)(f))

    def reverse: Seq[A] =
      foldr(Seq.empty)(a => l => l ++ Seq(a))

    def length: Int =
        def loop(l: Seq[A], acc: Int = 0): Int =
          l match
            case End()    => acc
            case Cel(_,r) => loop(r, acc + 1)
        loop(this)

    def size: Int =
      length

    def mkString: String =
      foldl("")(b => a => b + a)

    def mkString(lhs: String, sep: String, rhs: String): String =
      s"${lhs}${foldl("")(b => a => (b + sep + a)).drop(sep.length)}${rhs}"

    case End[Nothing]()                                    extends Seq[Nothing]
    case Cel[A](override val a: A, override val r: Seq[A]) extends Seq[A]

  object Seq:

    val end: Seq[Nothing] =
      End()

    def empty[A]: Seq[A] =
      End()

    def concat[A](l: Seq[A], r: Seq[A]): Seq[A] =
      l ++ r

      // scala library convertions

    import scala.List

    def apply[A](as: A*): Seq[A] =
      apply(as.toSeq)

    def apply[A](l: Iterable[A]): Seq[A] =
      l.foldRight(Seq.empty)((a,s) => Cel(a,s))

    def unapplySeq[A](sa: Seq[A]): Option[List[A]] =
      Some(sa.foldr(List.empty)(a => b => a :: b))
