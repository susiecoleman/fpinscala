package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(b) => Left(b)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(b) => Left(b)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => Right(a)
   case Left(_) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
   case Right(a) => b.map(f(a,_))
   case Left(l) => Left(l)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object MainEither {

  def test(x: Int): Int = x + 3

  def evenIsGreat(x: Int) = if (x % 2 == 0) Right(x) else Left("error")

  def main(args: Array[String]): Unit = {
    assert(Right(5).map(_ + 6) == Right(11))
    assert(Left(5).map(test) == Left(5))

    assert(Right("hello").flatMap(_ => Right("goodbye")) == Right("goodbye"))
    assert(Left(5).flatMap(_ => Right("goodbye")) == Left(5))

    assert(Left(5).orElse(Left(12)) == Left(12))
    assert(Right("hi").orElse(Left(12)) == Right("hi"))

    assert(Right("hello").map2(Right("goodbye"))((x,y) => s"$x $y") == Right("hello goodbye"))
    assert(Left(5).map2(Right("goodbye"))((x,y) => s"$x $y") == Left(5))
    assert(Left(5).map2(Left(10))((x,y) => s"$x $y") == Left(5))
    assert(Right("hello").map2(Left(10))((x,y) => s"$x $y") == Left(10))

    val l1 = List(1,2)
    val l2 = List(2,4)

    assert(Either.traverse(l1)(evenIsGreat) == Left("error"))
    assert(Either.traverse(l2)(evenIsGreat) == Right(List(2,4)))
  }
}