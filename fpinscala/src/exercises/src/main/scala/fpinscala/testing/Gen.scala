package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.state._
import Prop._
import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  // same as Prop.run
  //    run(this) match {
  //      case Passed => Right(100)
  //      case Falsified(failure, success) => Left((failure, success))
  //    }

  // 8.3
  // 8.9
  def &&(p: Prop): Prop =
  Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case f => f
      }
  }

  //  Prop {
  //    (max, n, rng) =>
  //      this.check match {
  //        case true => p.run(max, n, rng)
  //        case false => Falsified
  //      }
  //  }

  def ||(p: Prop): Prop =
    Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Passed => Passed
          case Proved => Proved
          case f => p.run(max, n, rng)
        }
    }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  // 这个forall是利用一个Stream[A]来对单个的随机生成的A进行测试
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop {
      (max, n, rng) =>
        randomStream(gen)(rng).zipAll(Stream.from(0)).take(n min max).mapR {
          case (Some(a), Some(i)) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }.find(_.isFalsified).getOrElse(Passed)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.s)(f)

  // 这里在对单个生成A的基础forAll封了一层，对不同长度的Gen传入基础forAll分别进行测试？
  // 这里为啥要搞个casesPerSize呢？似乎只是针对TestCases大于MaxSize的情况做的用例切分；
  // update：主要似乎是为了方便定位最小测试用例集；
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + max - 1) / max
      val props: Stream[Prop] = Stream.from(0).take(n min max + 1).mapR(i => forAll(g(i))(f))
      val prop: Prop = props.mapR(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop =
    Prop {
      (_, _, _) => if (p) Proved else Falsified("()", 0)
    }

  // 8.15
  // ???不懂这个check在干啥；update 类似断言的东西

  val S = Gen.weighted(Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75, Gen.unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def checkPar(p: => Par[Boolean]): Prop =
//    forAll(S)(p(_).get)
    Prop {
      (_, _, _) => if (p(Executors.newCachedThreadPool).get) Proved else Falsified("()", 0)
    }

  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    // 8.1
    val sumProp = forAll(Gen.listOf1(smallInt)) {
      ns => ns.sum == ns.reverse.sum
    }
    //    sumProp.check
    run(sumProp)
    // 8.2
    val maxProp = forAll(Gen.listOf1(smallInt)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    val failingProp = forAll(Gen.listOf1(smallInt)) {
      ns => ns == ns.reverse
    }
    run(maxProp)
    run(sumProp && maxProp)
    run(sumProp || maxProp)
    run(check(true))
    run(failingProp)
    val parProp = forAllPar(Gen.choose(1,5))(n => Par.map(Par.unit(n))(n2 => n2 >= 0 && n2 <= 5))
    run(parProp)
    run(checkPar(Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )))
    // 8.17
    run(checkPar(Par.equal(
      Par.fork(Par.unit(1)),
      Par.unit(1)
    )))
    // 8.18

  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

case object Proved extends Result {
  def isFalsified: Boolean = ???
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

object Gen {
  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // 8.5
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.7
  // 怎么聚合两个A变成一个A？需要一个(A, A) => A的函数吧；
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
  boolean.flatMap(if (_) g1 else g2)

  // 8.8
  // 泛化类型A怎么取得不同量的值？……
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
  Gen(State(RNG.double)).flatMap(d => if (d < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  // 8.16
  // todo Par[Int]的富生成器是指？

  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(10000)
    val g = Gen(State(RNG.int))
//    println(g.sample)
//    listOfN(10, g).sample.run(rng)._1.foreach(println)
//    listOfN(10, boolean).sample.run(rng)._1.foreach(println)
    val chooseProp = forAll(Gen.listOf1(Gen.choose(2, 9))) {
      ns => ns.forall(n => n >= 2 && n < 9)
    }
    run(chooseProp)
    val unitProp = forAll(Gen.listOf1(Gen.unit(42))) {
      ns => ns.forall(n => n == 42)
    }
    run(unitProp)
    listOfN(3, g).sample.run(rng)._1.foreach(println)
  }
}

// 根据长度返回生成器，这个是啥操作
// 其实是返回包含随机元素的定长集合（列表等）
case class SGen[A](s: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(n => s(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => s(n).flatMap {
      a => f(a).s(n)
    })
}

