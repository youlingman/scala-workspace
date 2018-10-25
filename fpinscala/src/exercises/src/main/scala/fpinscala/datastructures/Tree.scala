package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size(node: Tree[_]): Int = node match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(node: Tree[Int]): Int = node match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // 3.27
  def depth(node: Tree[_]): Int = node match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  // 3.28
  def map[A, B](node: Tree[A])(f: A => B): Tree[B] = node match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A, B](node: Tree[A])(f: A => B)(g: (B, B) => B): B = node match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeF(node: Tree[_]): Int = fold(node)(_ => 1)((a, b) => 1 + a + b)

  def maximumF(node: Tree[Int]): Int = fold(node)(v => v)(_ max _)

  def depthF(node: Tree[Int]): Int = fold(node)(_ => 1)((a, b) => 1 + a max b)

  def mapF[A, B](node: Tree[A])(f: A => B): Tree[B] = fold(node)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

  def main(args: Array[String]): Unit = {
    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(sizeF(Leaf(1)) == 1)
    assert(sizeF(Branch(Leaf(1), Leaf(2))) == 3)
    assert(map(Branch(Leaf(1), Leaf(2)))(_ * 2)  == Branch(Leaf(2), Leaf(4)))
    assert(mapF(Branch(Leaf(1), Leaf(2)))(_ * 2)  == Branch(Leaf(2), Leaf(4)))
    println("All PASS!")
  }
}