package proust

case class P[A](parse: String => List[(A,String)]) {

  def flatMap[B](f: A => P[B]): P[B] =
    P{ s => parse(s).map((a,ss) => f(a).parse(ss)).fold(Nil)(_ ++ _) }

  def map[B](f: A => B): P[B] =
    P{ s => parse(s).map((a, ss) => (f(a), ss)) }
}

object P {

  def run[A](p: P[A])(s: String): A =
    p.parse(s) match {
      case List((a, "")) => a
      case List((_, rs)) => sys.error(s"Unconsumed: ${rs.take(10)}...")
      case _             => sys.error(s"Parser error")
    }

  def item: P[Char] =
    P(s => if (s.length == 0) Nil else List((s.head, s.tail)))

  def unit[A](a: A): P[A] = 
    P(s => List((a, s)))

  def zero[A]: P[A] =
    P(_ => Nil)

  def option[A](l: P[A])(r: P[A]): P[A] =
    P(s => l.parse(s) match {
      case Nil => r.parse(s)
      case res => res
    })

  def combine[A](l: P[A], r: P[A]): P[A] =
    P(s => l.parse(s) ++ r.parse(s))

  def satisfy(p: Char => Boolean): P[Char] =
    item.flatMap(c => if p(c) then unit(c) else zero)

  def oneOf(s: String): P[Char] =
    s.map(c => satisfy(_ == c)).fold(zero)(combine)
}
