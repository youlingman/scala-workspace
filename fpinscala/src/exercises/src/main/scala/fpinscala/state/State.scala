package fpinscala.state

import sun.java2d.xr.XRSurfaceData.XRInternalSurfaceData


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (Math.abs(i % Int.MaxValue), r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (Math.abs(i.toDouble / Int.MaxValue) % 1, r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (l, r2) = ints(count - 1)(r)
      (i :: l, r2)
    }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt
    (i % 2 == 0, r)
  }

  // 6.5
  def doubleM(rng: RNG): (Double, RNG) =
    map(int) {
      i => Math.abs(i.toDouble / Int.MaxValue) % 1
    }(rng)

  def nonNegativeIntM(rng: RNG): (Int, RNG) =
    map(int) {
      i => Math.abs(i % Int.MaxValue)
    }(rng)

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng) => {
      // 串行状态转换
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
      // 也可以采取如下的并行转换策略
      // val (a, r1) = ra(rng)
      // val (b, r2) = rb(rng)
      // (f(a, b), pick(r1, r2))
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleM(rng: RNG): ((Int, Double), RNG) =
    both(int, double)(rng)

  def doubleIntM(rng: RNG): ((Double, Int), RNG) =
    both(double, int)(rng)

  // 6.7
  //
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => {
    //      val l = fs.map(r => r(rng))
    //      (l.map(_._1), l.takeRight(1).head._2)
    fs.foldRight((List[A](), rng)) { case (itR, (ls, seedR)) => (itR(seedR)._1 :: ls, itR(seedR)._2) }
  }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // 6.9
  def mapF[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {
    val r1 = Simple(42)
    val r2 = Simple(54)
    assert(map2(_.nextInt, _.nextInt)(_ + _)(r1)._1 == map2(_.nextInt, _.nextInt)(_ + _)(r1)._1)
    assert(sequence(List.fill(5)(int))(r1)._1 == sequence(List.fill(5)(int))(r1)._1)
    println(sequence(List.fill(5)(int))(r1))
    println(sequence(List.fill(5)(int))(r2))
    println("ALL PASS!")
  }
}

case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def map[B](f: A => B): State[S, B] =
  //    State(s => {
  //      val (a, s2) = run(s)
  //      (f(a), s2)
  //    })
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
  //    State(s => {
  //      val (a, s1) = run(s)
  //      val (b, s2) = sb.run(s1)
  //      (f(a, b), s2)
  //    })
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // 6.10
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List())) { (s, acc) => s.map2(acc)(_ :: _) }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State({
      m =>
        packageMachine(inputs.foldLeft(m) {
          simulateInput
        })
    })

  def packageMachine(m: Machine): ((Int, Int), Machine) =
    m match {
      case Machine(_, candies, coins) => ((candies, coins), m)
    }

  def simulateInput(m: Machine, input: Input): Machine =
    m match {
        // unlocked state
      case Machine(false, candies, coins) => input match {
        case Coin => Machine(false, candies, coins) // 非锁定状态投币，什么也不做
        case Turn => Machine(true, candies - 1, coins) // 非锁定状态按按钮，给出糖果，转到锁定状态
      }
        // locked state
      case Machine(true, candies, coins) => input match {
        case Coin => if(candies > 0) Machine(false, candies, coins + 1) else Machine(true, candies, coins)  // 锁定状态投币，如果有剩余糖果则转入非锁定状态，如果无剩余糖果就直接吞币？……先按吐币处理吧
        case Turn => Machine(true, candies, coins)  // 锁定状态按按钮，什么也不做
      }
    }

  def main(args: Array[String]): Unit = {
    val ins = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val m = Machine(locked = true, 5, 10)
    assert(simulateMachine(ins).run(m)._2 == Machine(true, 1, 14))
    println("All PASS!")
  }
}
