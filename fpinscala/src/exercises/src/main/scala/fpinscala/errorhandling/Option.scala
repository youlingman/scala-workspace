package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) this else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(v => math.pow(v - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (v1 => b map (v2 => f(v1, v2)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (v => sequence(t) map (v :: _))
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (b => traverse(t)(f) map (b :: _))
  }

  def main(args: Array[String]): Unit = {
    assert(Some(5).map(_ * 2) == Some(10))
    assert(None.map(_ => 5) == None)
    assert(Some(3).getOrElse(5) == 3)
    assert(None.getOrElse(5) == 5)
    assert(Some(5).flatMap(Some(_)) == Some(5))
    assert(Some(5).flatMap(_ => None) == None)
    assert(None.flatMap(_ => Some(5)) == None)
    assert(Some(5).orElse(None) == Some(5))
    assert(None.orElse(Some(5)) == Some(5))
    assert(None.orElse(None) == None)
    assert(variance(List(3.0, 6.0, 9.0)) == Some(6.0))
    assert(variance(List()) == None)
    assert(variance(Nil) == None)
    assert(map2(Some(2), None)((_, _) => 2) == None)
    assert(map2(None, Some(2))((_, _) => 2) == None)
    assert(map2(Some(2), Some(3))(_ + _) == Some(5))
    assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(List(1, 2, 3, 4)))
    assert(sequence(List(Some(1), Some(2), None, Some(4))) == None)
    assert(traverse(List(1, 2, 3, 4)){ v => Some(v * 2) } == Some(List(2, 4, 6, 8)))
    println("ALL PASS!")
  }
}