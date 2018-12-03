package fpinscala.parallelism

import java.util.concurrent._

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es =>
      es.submit(
        new Callable[A] {
          def call = a(es).get
        })

  // 7.9
  // 为何fork的实现会造成死锁？根本在于fork实现向线程池提交一个callable，此callable会占用一个线程直到callable执行结束，
  // 而callable的执行中，可能也需要向线程池提交task并等待该task执行完毕后返回，该callable才能执行结束，
  // 往线程池提交任务和等待返回有点类似函数调用的情况，固定大小的线程池无法避免死锁的出现（固定内存遇到无限递归调用的情况无法避免爆栈）

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.3 map2 with timeout
  def map2T[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Long): Par[Option[C]] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(try {
        val c = f(af.get(timeout, TimeUnit.MILLISECONDS), bf.get(timeout, TimeUnit.MILLISECONDS))
        Some(c)
      } catch {
        case e => None
      })
    }

  // 7.4
  def asyncF[A, B](f: (A) => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, sequence(t))(_ :: _)
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fas = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(fas))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // 7.11
  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
    es => ps(run(es)(p).get)(es)

  // 7.12
  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
    es => ps(run(es)(p).get)(es)

  // 7.13
  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => f(run(es)(p).get)(es)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    map2(p, p2)(_ == _)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(5)
    val a = lazyUnit(43)
    val b = lazyUnit(3)
    val c = asyncF { a: Int => a + 1 }(5)
    assert(map2(a, b)(_ + _)(es).get == 46)
    assert(map2(b, c)(_ + _)(es).get == 9)
    assert(sequence(List(a, b, c))(es).get == List(43, 3, 6))
    assert(parFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0)(es).get == List(2, 4))
    assert(choice(unit(true))(a, b)(es).get == 43)
    assert(choice(unit(false))(a, b)(es).get == 3)
    assert(choiceN(unit(0))(List(a, b, c))(es).get == 43)
    assert(choiceN(unit(1))(List(a, b, c))(es).get == 3)
    assert(choiceN(unit(2))(List(a, b, c))(es).get == 6)
    println("ALL PASS!")
    val af = a(es)
    println(af)
    println(af.get)
    val fa = fork(a)(es)
    // deadlock by nest submitting task to ExecutorService in fork
    // only for SingleThreadExecutor
    println(fa)
    // call get to jump into the deadlock blocking
    println(fa.get)
    //    println(Par.equal(es)(a, fork(a)))
    es.shutdown()
  }

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
