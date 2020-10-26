package proust

object threading {

case class State[I,O,A](run: I => (A,O)) {

  def map[B](f: A => B): State[I,O,B] =
    flatMap(a => State(s => (f(a),s)))

  def flatMap[B,E](f: A => State[O,E,B]): State[I,E,B] =
    State(s => { val (a,ss) = run(s) ; f(a).run(ss) })
}
  
object State {

  def pure[I,A](a: A): State[I,I,A] =
    State(i => (a,i))

  def inspect[I,A](fa: I => A): State[I,I,A] =
    State(i => (fa(i),i))

  def bimap[I,O,A](fa: I => A, fo: I => O): State[I,O,A] =
    State(i => (fa(i),fo(i)))
  
  def get[I]: State[I,I,I] =
    inspect(identity)

  def set[O](o: O): State[_,O,Unit] =
    State(_ => ((), o))

  def modify[I,O](fo: I => O): State[I,O,Unit] =
    State(i => ((), fo(i)))
}
}