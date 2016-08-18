sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Ex 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1;
    case Branch(left, right) => 1 + size(left) + size(right);
  }

  // Ex 3.26
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  // Ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  // Ex 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Ex 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree)((a) => 1)((b1: Int, b2: Int) => b1 + b2 + 1)

  def maxFold(tree: Tree[Int]): Int =
    fold[Int, Int](tree)((a) => a)((b1, b2) => b1 max b2)

  def depthFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(a => 0)((b1, b2) => 1 + (b1 max b2))

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(a => Leaf(f(a)))((b1, b2) => Branch(b1, b2))
}

// Rez Ex 3.25
val sizeRez = Tree.size[Int](Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
// Rez Ex 3.26
val maxRez = Tree.max(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
// Rez Ex 3.27
val depthRez = Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(2), Leaf(3)))))
// Rez Ex 3.28
val mapRez = Tree.map[Int, Int](Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(2), Leaf(3)))))(a => a + 1)
// Rez Ex 3.29
val sizeFoldRez = Tree.sizeFold[Int](Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
val maxFoldRez = Tree.maxFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(3))))
val depthFoldRez = Tree.depthFold(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(2), Leaf(3)))))
val mapFoldRez = Tree.mapFold[Int, Int](Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(2), Leaf(3)))))(a => a + 1)
