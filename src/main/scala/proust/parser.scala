package proust

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
  
  def |~|[B](that: P[B]): P[B] =
    for { _ <- this ; b <- that } yield b
    
  def foldLeft[B](b: B)(f: B => A => B): P[B] =
    P(s => for { (a,s1) <- parse(s) } yield (f(b)(a), s1))

  def chainl(pf: P[A => A => A])(a: A): P[A] =
    chainl1(pf) |!| unit(a)

  def chainl1(pf: P[A => A => A]): P[A] = {
    def rest(a: A): P[A] = (for { f <- pf ; b <- this } yield f(a)(b)) |!| unit(a)
    for { a <- this ; r <- rest(a) } yield r
  }

  private def rest(s: String, acc: List[A]): (List[A], String) =
    parse(s) match {
      case Nil => (acc.reverse, s)
      case List((a,ss)) => rest(ss, a :: acc)
      case l            => sys.error(s"Multiple results: ${l}")
    }

  // One or more
  def some: P[List[A]] =
    P(s => for { (a,s1) <- parse(s) } yield rest(s1, List(a)))
    
  // Zero or more
  def many: P[List[A]] =
    P(s => List(rest(s, Nil)) )
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

  def spaces: P[String] =
    oneOf(" \t\n\r").many.map(_.mkString)

  def token[A](p: P[A]): P[A] =
    for { a <- p ; _ <- spaces } yield a

  def reserved(keyword: String): P[String] =
    token(string(keyword))

  def digit: P[Char] =
    satisfy(_.isDigit)

  def number: P[Int] =
    for {
      s <- string("-") |!| unit("")
      r <- digit.some
    } yield (s + r.mkString).toInt

  def parens[A](pa: P[A]): P[A] =
    for {
      _ <- reserved("(")
      a <- pa
      _ <- reserved(")")
    } yield a 

  object calculator {

    sealed trait Expr
    case class Add(l: Expr, r: Expr) extends Expr
    case class Sub(l: Expr, r: Expr) extends Expr
    case class Mul(l: Expr, r: Expr) extends Expr
    case class Div(l: Expr, r: Expr) extends Expr
    case class Lit(v: Int)           extends Expr

    def eval(s: String): Int =
      eval(parse(s))

    def eval(e: Expr): Int = e match {
      case Add(l, r) => eval(l) + eval(r)
      case Sub(l, r) => eval(l) - eval(r)
      case Mul(l, r) => eval(l) * eval(r)
      case Div(l, r) => eval(l) / eval(r)
      case Lit(v)    => v
    }

    // number = [ "-" ] digit { digit }
    // digit  = "0" | "1" | ... | "8" | "9"
    // expr   = term { addop term }
    // term   = factor { mulop factor }
    // factor = "(" expr ")" | number
    // addop  = "+" | "-"
    // mulop  = "*" | "/"

    def int: P[Expr] =
      for { n <- number } yield Lit(n)

    def expr: P[Expr] =
      term.chainl1(addOp)

    def term: P[Expr] =
      factor.chainl1(mulOp)

    def factor: P[Expr] =
      int |!| parens(expr)

    def addOp: P[Expr => Expr => Expr] =
      infixOp("+", l => r => Add(l, r)) |!| infixOp("-", l => r => Sub(l, r))

    def mulOp: P[Expr => Expr => Expr] =
      infixOp("*", l => r => Mul(l, r)) |!| infixOp("/", l => r => Div(l, r))

    def infixOp(s: String, f: Expr => Expr => Expr): P[Expr => Expr => Expr] =
      reserved(s) |~| unit(f)
        
    def parse(s: String): Expr =
      run(expr)(s)
  
  }
}
