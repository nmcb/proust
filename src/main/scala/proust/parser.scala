package proust

// import scala.language.postfixOps

case class P[A](parse: String => List[(A,String)]) {

  def identity: P[A] =
    this

  def unit: P[A] =
    identity

  def unit[B](b: B): P[B] =
    P.unit(b)

  def fail: P[A] =
    P.fail

  def bind[B](f: A => P[B]): P[B] =
    P(s => parse(s).map((a,ss) => f(a).parse(ss)).fold(Nil)(_ ++ _))

  def flatMap[B](f: A => P[B]): P[B] =
    bind(f)

  def map[B](f: A => B): P[B] =
    P(s => parse(s).map((a, ss) => (f(a), ss)))

  def ap[B](ff: P[A => B]): P[B] =
    P(s => for { (f, s1) <- ff.parse(s) ; (a, s2) <- parse(s1) } yield (f(a), s2))

  def |!|(that: => P[A]): P[A] =
    P(s => parse(s) match {
      case Nil => that.parse(s)
      case res => res
    })

  def |&|(that: P[A]): P[A] =
    P(s => parse(s) ++ that.parse(s))

  def fold[B](b: B)(pf: P[A => B => B]): P[B] = {
    def rest(a: A): P[B] = (for { f <- pf ; a <- this } yield f(a)(b) ) |!| unit(b)
    for { a <- this ; r <- rest(a) } yield r
  }

  def chainl(pf: P[A => A => A]): P[A] =
    for { a <- this ; r <- fold(a)(pf) } yield r

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
  
  def item: P[Char] =
    P(s => if (s.length == 0) Nil else List((s.head, s.tail)))

  def fail[A]: P[A] =
    P(_ => Nil)

  def combine[A](l: P[A], r: P[A]): P[A] =
    l |&| r

  def satisfy(p: Char => Boolean): P[Char] =
    item.bind(c => if p(c) then unit(c) else fail)

  def oneOf(s: String): P[Char] =
    s.map(c => satisfy(_ == c)).fold(fail)(combine)

  def char(c: Char): P[Char] =
    satisfy(_ == c)

  def string(s: String): P[String] =
    if (s.isEmpty) then unit("") else for { _ <- char(s.head) ; _ <- string(s.tail) } yield s

  def digit: P[Char] =
    satisfy(_.isDigit)

}
