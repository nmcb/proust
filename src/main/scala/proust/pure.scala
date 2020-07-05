package proust

trait Functor[F[_]] {

  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  
  def pure[A](a: A): F[A]
  
  def ap[A,B](f: F[A => B])(fa: F[A]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)
}