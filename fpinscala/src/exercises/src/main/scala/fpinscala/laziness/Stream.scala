package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.1 toList helper
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) Empty else cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => if (n <= 0) this else t().drop(n - 1)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty => true
  }

  // 5.5
  def takeWhileR(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else Empty)

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def mapR[B](f: A => B): Stream[B] =
  foldRight(empty[B])((h, t) => cons(f(h), t))

  def filterR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def appendR[B >: A](rest: => Stream[B]): Stream[B] =
    foldRight(rest)((h, t) => cons(h, t))

  def flatMapR[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) appendR t)

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) => (h1() == h2()) && t1().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }

  // 5.13
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), nn) => if (nn > 0) Some((h(), (t(), nn - 1))) else None
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) => if (p(h())) Some((h(), t())) else None
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  // 5.14
  def startsWithZ[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2 contains Some(empty[B])).forAll { case (h1, h2) => h1 == h2 }

  // 5.15
  // add additional flag to terminate
  def tails: Stream[Stream[A]] =
  //    unfold(this, false) {
  //      case (_, true) => None
  //      case (Empty, false) => Some((empty, (empty, true)))
  //      case (Cons(h, t), false) => Some((Cons(h, t), (t(), false)))
  //    }
  unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((Cons(h, t), t()))
  } appendR Stream(empty)

  // 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.mapR(_.foldRight(z)(f))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs: Stream[Int] = {
    def hepler(a: Int, b: Int): Stream[Int] =
      cons(a + b, hepler(b, a + b))

    cons(0, cons(1, hepler(0, 1)))
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // 5.12
  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(v => Some((v, v + 1)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(v => Some((v, v)))

  val onesUnfold: Stream[Int] = constantUnfold(1)

  def main(args: Array[String]): Unit = {
    val list = fibs.take(5)
    assert(list.toList == List(0, 1, 1, 2, 3))
    assert(list.take(3).toList == Stream(0, 1, 1).toList)
    assert(list.drop(2).toList == Stream(1, 2, 3).toList)
    assert(list.takeWhile(_ <= 1).toList == Stream(0, 1, 1).toList)
    assert(list.forAll(_ >= 0))
    assert(list.forAll(_ <= 3))
    assert(list.takeWhileR(_ <= 1).toList == Stream(0, 1, 1).toList)
    assert(list.headOption.contains(0))
    assert(Empty.headOption.isEmpty)
    assert(list.mapR(_ * 2).toList == List(0, 2, 2, 4, 6))
    assert(list.filterR(_ < 2).toList == List(0, 1, 1))
    assert(list.appendR(list).toList == List(0, 1, 1, 2, 3, 0, 1, 1, 2, 3))
    assert(list.flatMapR(Stream(_)).toList == List(0, 1, 1, 2, 3))
    assert(fibs.take(10).toList == fibsUnfold.take(10).toList)
    assert(from(1).take(10).toList == fromUnfold(1).take(10).toList)
    assert(constant(1).take(10).toList == constantUnfold(1).take(10).toList)
    assert(list.takeUnfold(3).toList == Stream(0, 1, 1).toList)
    assert(list.mapUnfold(_ * 2).toList == List(0, 2, 2, 4, 6))
    assert(list.takeWhileUnfold(_ <= 1).toList == Stream(0, 1, 1).toList)
    assert(list.zipWith(Stream(1, 1, 1, 1, 1))(_ + _).toList == Stream(1, 2, 2, 3, 4).toList)
    assert(list.zipAll(Stream(0, 1, 2)).toList == Stream((Some(0), Some(0)), (Some(1), Some(1)), (Some(1), Some(2)), (Some(2), None), (Some(3), None)).toList)
    assert(list.startsWith(Stream(0)))
    assert(list.startsWith(Stream(0, 1)))
    assert(list.startsWith(Stream(0, 1, 1)))
    assert(list.startsWith(Stream(0, 1, 1, 2, 3)))
    assert(!list.startsWith(Stream(0, 1, 2)))
    assert(list.tails.mapR(_.toList).toList == List(List(0, 1, 1, 2, 3), List(1, 1, 2, 3), List(1, 2, 3), List(2, 3), List(3), List()))
    assert(Stream(0).tails.mapR(_.toList).toList == List(List(0), List()))
    assert(Empty.tails.mapR(_.toList).toList == List(List()))
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
    println("All PASS!")
  }
}