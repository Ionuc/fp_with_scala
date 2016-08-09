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
  def max(tree: Tree[Int]): Int = {
    def g(tree: Tree[Int])(maxV: Int): Int = tree match {
      case Leaf(value) => value max maxV
      case Branch(left, right) => g(left)(maxV) max g(right)(maxV)
    }
    g(tree)(Int.MinValue)
  }

  // Ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
  }

  // Ex 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Ex 3.29
  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) => fold(left, fold(right, z)(f))(f)
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree, 0)((a, v) => v + 1)

  def maxFold(tree: Tree[Int]): Int =
    fold(tree, Int.MinValue)((a : Int, b : Int) => a max b)

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree, null)((a : A, b : B) =>  f(a))
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
val maxFoldRez = Tree.maxFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))