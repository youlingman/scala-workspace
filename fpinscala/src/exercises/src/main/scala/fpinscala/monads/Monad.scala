package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds
import language.implicitConversions


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

// 映射法则 map(a)(x => x) === a
// 组合法则 compose(compose(f , g), h) === compose(f, compose(g , h))
// 单位元法则 compose(f, unit) === compose(unit, f) === f
trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(m => m)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = la match {
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    case Nil => unit(Nil)
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  // 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List(): List[A]))((a, acc) => map2(f(a), acc)((b, l) => if (b) l else a :: l))

  // 11.7
  // kleisli箭头结合率
  // compose(compose(f , g), h) === compose(f, compose(g , h))
  // compose(f, unit) === compose(unit, f) === f
  // flatMap(f(a))(unit) === flatMap(unit(a))(f) === f(a)
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
  a => flatMap(f(a))(g)

  // 11.8
  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)(Unit)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(m => m)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.chooser(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def parserMonad[P[+ _]](p: Parsers[ParseError, P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  // 11.16
  // listMonad的identity法则：flatMap(f(a))(List(_)) === flatMap(List(a))(f) === f(a)
  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)

    override def unit[A](a: => A): List[A] = List(a)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }

  def getState[S]: State[S, S] = State(s => (s, s))


  def setState[S](s: S): State[S, Unit] = State(_ => (Unit, s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
  //    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
  //      xs <- acc
  //      n <- getState
  //      _ <- setState(n + 1)
  //    } yield (n, a) :: xs).run(0)._1.reverse
  // 这里其实是利用flatMap绑定变量实现状态传递的过程
  // 感觉有点对上下文作pattern matching的感觉
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => acc.flatMap(
      xs =>
        // state的flatmap其实是对状态进行串联
        // 这里getstate是抽取State(s => (a, s))中的s进行处理
        getState.flatMap(
          // 这里的n就是传入的启动值，即传入run的0，然后利用setstate更新S，再利用map更新状态的值A
          n => setState(n + 1).map(_ => (n, a) :: xs)))
    ).run(0)._1.reverse

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f

    override def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = ???

  def main(args: Array[String]): Unit = {
    println(zipWithIndex(List(4,3,2,1)))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
  }
}

