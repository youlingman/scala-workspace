package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for (
      aa <- this;
      bb <- b
    ) yield f(aa, bb)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  // 4.7
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  // 4.8
  // map2展开时，flatmap和map在当前为Left时会忽略下一层的Left信息，参考java的异常堆栈，可以把签名调整成返回一个‘异常’List
  // 然后在map和flatmap处把Left的e作concat？

  def main(args: Array[String]): Unit = {
    assert(Right(3).map(_ + 2) == Right(5))
    assert(Left(3).map(_ => 5) == Left(3))
    assert(Right(2).map2(Left(1))((_, _) => 2) == Left(1))
    assert(Left(1).map2(Right(2))((_, _) => 2) == Left(1))
    assert(Right(2).map2(Right(1))(_ + _) == Right(3))
    println("ALL PASS!")
  }
}