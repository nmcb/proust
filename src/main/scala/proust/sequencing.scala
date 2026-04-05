package proust

import scala.annotation.tailrec

object sequencing:

  import disjoining.*
  import option.*

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

    def foldLeft[B](b: B)(f: B => A => B): B =
      this match
        case End()    => b
        case Cel(a,r) => r.foldLeft(f(b)(a))(f)

    def foldRight[B](b: B)(f: A => B => B): B =
      this match
        case End()    => b
        case Cel(a,r) => f(a)(r.foldRight(b)(f))

    def reverse: Seq[A] =
      foldRight(Seq.empty)(a => l => l ++ Seq(a))

    def length: Int =
        @tailrec
        def loop(l: Seq[A], acc: Int = 0): Int =
          l match
            case End()    => acc
            case Cel(_,r) => loop(r, acc + 1)
        loop(this)

    def size: Int =
      length

    def mkString: String =
      foldLeft("")(b => a => b + a)

    def mkString(lhs: String, sep: String, rhs: String): String =
      s"$lhs${foldLeft("")(b => a => b + sep + a).drop(sep.length)}$rhs"

    case End()                                          extends Seq[Nothing]
    case Cel(override val a: A, override val r: Seq[A]) extends Seq[A]

  object Seq:
    
    val end: Seq[Nothing] =
      End()

    def empty[A]: Seq[A] =
      End()

    def concat[A](l: Seq[A], r: Seq[A]): Seq[A] =
      l ++ r

    // scala library conversions

    import scala.List

    def apply[A](as: A*): Seq[A] =
      apply(as.toSeq)

    def apply[A](l: Iterable[A]): Seq[A] =
      l.foldRight(Seq.empty)((a,s) => Cel(a,s))

    def unapplySeq[A](sa: Seq[A]): Option[List[A]] =
      Some(sa.foldRight(List.empty)(a => b => a :: b))
