package proust

case class P[A](parse: String => List[(A,String)]) {

  def bind[B](f: A => P[B]): P[B] =
    P(s => parse(s).map((a,ss) => f(a).parse(ss)).fold(Nil)(_ ++ _))

  def flatMap[B](f: A => P[B]): P[B] =
    bind(f)

  def map[B](f: A => B): P[B] =
    P(s => parse(s).map((a, ss) => (f(a), ss)))

  def ap[A](p: P[A]): P[A] =
    ???
}

object P {

  def run[A](p: P[A])(s: String): A =
    p.parse(s) match {
      case List((a, "")) => a
      case List((_, rs)) => sys.error(s"Unconsumed: $rs")
      case e             => sys.error(s"Parser error: $e")
    }

  def unit[A](a: A): P[A] = 
    P(s => List((a, s)))
  
  implicit def pFunctor[A]: Functor[P] =
    new Functor[P] {
      def map[A,B](pa: P[A])(f: A => B): P[B] =
        P(s => pa.parse(s).map((a, ss) => (f(a), ss)))
    }
    
  implicit def pApplicative[A]: Applicative[P] =
    new Applicative[P] {
      def pure[A](a: A): P[A] =
        unit(a)
      def ap[A,B](pf: P[A => B])(fa: P[A]): P[B] =
        P(s => pf.parse(s).flatMap((f,s1) => fa.parse(s1).map((a, s2) => (f(a), s2))))
    }

  def item: P[Char] =
    P(s => if (s.length == 0) Nil else List((s.head, s.tail)))

  def empty[A]: P[A] =
    P(_ => Nil)

  def option[A](l: P[A])(r: P[A]): P[A] =
    P(s => l.parse(s) match {
      case Nil => r.parse(s)
      case res => res
    })

  def combine[A](l: P[A], r: P[A]): P[A] =
    P(s => l.parse(s) ++ r.parse(s))

  def satisfy(p: Char => Boolean): P[Char] =
    item.bind(c => if p(c) then unit(c) else empty)

  def oneOf(s: String): P[Char] =
    s.map(c => satisfy(_ == c)).fold(empty)(combine)

  def char(c: Char): P[Char] =
    satisfy(_ == c)

  def string(s: String): P[String] =
    if (s.isEmpty) then unit("") else for { _ <- char(s.head) ; _ <- string(s.tail) } yield s

}
