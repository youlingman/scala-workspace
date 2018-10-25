package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
  if (n <= 0) l
  else drop(tail(l), n - 1)

  // 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // 3.7
  // product2(List(0.0))
  // foldRight(List(0.0), 1.0)(_ * _)
  // 0.0 * foldRight(Nil, 1.0)(_ * _)
  // 0.0 * 1.0
  // 0.0

  // 3.8
  // 基本是把List组装一遍

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, n) => n + 1 }

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 可作尾递归优化
  // sumL(List(1, 2, 3))
  // foldLeft(List(1, 2, 3), 0)(_ + _)
  // foldLeft(List(2, 3), (0 + 1))(_ + _)
  // foldLeft(List(3), (1 + 2))(_ + _)
  // foldLeft(Nil, (3 + 3))(_ + _)
  // 6

  // 3.11
  def sumL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0) { (n, _) => n + 1 }

  // 3.12
  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil: List[A])((ls, n) => Cons(n, ls))

  // 3.13
  // 所以foldRight是从左往右递归展开然后再从右往左递归归并，而foldLeft则是从左往右展开并就地evaluate（尾递归优化）
  def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  foldRight(l, z)((a, b) => f(b, a))

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldRight(as, z)((a, b) => f(a, b))

  // 3.14
  def appendR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2) { (n, ls) => Cons(n, ls) }

  //  def appendL[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2) { (ls, n) => Cons(n, ls) }

  // 3.15
  def flatten[A](ls: List[List[A]]): List[A] = foldLeft(ls, List(): List[A])(append)

  // 3.16
  def incList(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, incList(t))
  }

  // 3.17
  def dToStr(ls: List[Double]): List[String] = ls match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, dToStr(t))
  }

  // 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) filter(t)(f) else Cons(h, filter(t)(f))
  }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // 3.21
  def filterF[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) { a => if (f(a)) Nil else List(a) }

  // 3.22
  def zipAddInt(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAddInt(t1, t2))
  }

  // 3.23
  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl, hr), zipWith(tl, tr)(f))
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(hp, tp), Cons(hb, tb)) => (hp == hb) && hasSubsequence(tp, tb)
  }

  def main(args: Array[String]): Unit = {
    //    println(x)
    assert(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3, 4))
    assert(reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
    assert(append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    assert(appendR(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    assert(incList(List(1, 2, 3, 4)) == List(2, 3, 4, 5))
    assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
    assert(filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(1, 3))
    assert(filterF(List(1, 2, 3, 4))(_ % 2 == 0) == List(1, 3))
    assert(zipAddInt(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println("All PASS!")
  }
}
