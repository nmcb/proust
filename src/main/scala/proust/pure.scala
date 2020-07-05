package proust

trait Functor[F[_]] {

  def map[A,B](f: A => B)(fa: F[A]): F[B]
}

abstract class Applicative[F[_]] extends Functor[F] {
  
  def pure[A](a: A): F[A]
  
  def ap[A,B](f: F[A => B])(fa: F[A]): F[B]

  override def map[A,B](f: A => B)(fa: F[A]): F[B] =
    ap(pure(f))(fa)
}

abstract class Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

}

trait MonadPlus[F[_]] extends Monad[F] {

  def mzero[A]: F[A]
  
  def mplus[A](fl: F[A])(fr: F[A]): F[A]

}

trait Alternative[F[_]] extends Applicative[F] {
  
  def empty[A]: F[A]

  def or[A](fl: F[A])(fr: F[A]): F[A]

  def some[A](fa: F[A]): F[List[A]] =
    ap(map[A, List[A] => List[A]](a => l => if (l.isEmpty) List(a) else (a :: l))(fa))(many(fa))

  def many[A](fa: F[A]): F[List[A]] =
    or(some(fa))(pure(Nil))
}
