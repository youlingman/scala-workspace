package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.testing

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    val zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a => a2(a1(a))

    val zero: (A) => A = a => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): testing.Prop =
    Prop.forAll(gen)(a => m.op(a, m.zero) == m && m.op(m.zero, a) == m)
      //      .&&(Prop.forAll(Gen.listOfN(3, gen)) { case List(a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) })
      .&&(Prop.forAll(for {
      a <- gen
      b <- gen
      c <- gen
    } yield (a, b, c)) { case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) })

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  //    foldMap(as, endoMonoid[B])(f.curried)(z)

  // 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length <= 2) foldMap(as.toList, m)(f)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // 10.9
  // 序列要满足有序，只需要每个前后元素对满足有序即可；
  // 其实用fold比用monoid更合适吧；
  def ordered(ints: IndexedSeq[Int]): Boolean =
  //    ints.zip(ints.tail).forall(t => t._1 <= t._2)
  //    ints.foldLeft((Int.MinValue, true)) { case ((prev, ordered), i) => (i, ordered && prev <= i) }._2
  {
    val orderMonoid = new Monoid[Option[(Int, Boolean)]] {
      def op(a1: Option[(Int, Boolean)], a2: Option[(Int, Boolean)]) =
        (a1, a2) match {
          case (None, Some((i, p))) => Some((i, p))
          case (Some((i, p)), None) => Some((i, p))
          case (Some((i1, p1)), Some((i2, p2))) => Some((i2, p1 && i1 <= i2))
        }

      val zero = None
    }

    foldMap(ints.toList, orderMonoid)(i => Some((i, true))).getOrElse((0, true))._2
  }

  import fpinscala.parallelism.Nonblocking._

  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      def op(a1: Par[A], a2: Par[A]) = Par.map2(a1, a2)(m.op)

      val zero = Par.unit(m.zero)
    }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def _count(s: String): Int = if (s.isEmpty) 0 else 1

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + _count(r1 + l2), r2)
    }

    val zero: WC = Stub("")
  }

  def count(s: String): Int = {
    foldMapV(s, wcMonoid) {
      c =>
        if (c.isWhitespace) Part("", 0, "")
        else Stub(c.toString)
    } match {
      case Part(l, c, r) => c + _count(l) + _count(r)
      case Stub(s) => _count(s)
    }
  }

  // 10.16
  // (a1 aop a2) aop a3 == a1 aop (a2 aop a3)
  // (b1 bop b2) bop b3 == b1 bop (b2 bop b3)
  // ((a1 aop a2) aop a3, (b1 bop b2) bop b3) == (a1 aop (a2 aop a3), b1 bop (b2 bop b3))
  // ((a1, b1) pop (a2, b2)) pop (a3, b3) == (a1, b1) pop ((a2, b2) pop (a3, b3))
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
  new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    val zero: (A, B) = (A.zero, B.zero)
  }

  // 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1.apply(a), a2.apply(a))

      val zero: (A) => B = _ => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) {
          (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        }

      val zero: Map[K, V] = Map[K, V]()
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

  def main(args: Array[String]): Unit = {
    assert(ordered(List(1, 2, 3, 4, 5).toIndexedSeq))
    assert(ordered(List().toIndexedSeq))
    assert(ordered(List(1).toIndexedSeq))
    assert(!ordered(List(1, 3, 2).toIndexedSeq))
    assert(count("") == 0)
    assert(count("abc") == 1)
    assert(count(" abc") == 1)
    assert(count("abc ") == 1)
    assert(count(" abc ") == 1)
    assert(count("abc def") == 2)
    assert(bag(List(1, 1, 2, 3, 4, 4, 4).toIndexedSeq) == Map(1 -> 2, 2 -> 1, 3 -> 1, 4 -> 3))
  }
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(v) => f(v)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(v) => f(z, v)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(v) => f(v, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
}

