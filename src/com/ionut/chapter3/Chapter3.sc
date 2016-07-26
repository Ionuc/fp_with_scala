sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0;
    case Cons(x, xs) => x + sum(xs)
  }

  def products(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * products(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail(ds: List[Int]): Int = ds match {
    case Nil => 0
    case Cons(x, _) => x
  }

  def setHead(ds: List[Int], v: Int) = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(v, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) if n >= 0 => Nil
    case Cons(x, xs) if n == 0 => Cons(x, xs)
    case Cons(x, xs) => drop(xs, n - 1)
  }

  // Ex 3.5
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }


  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumFoldRight(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  // Ex 3.7
  def productFoldRight(ns: List[Int]) = foldRight(ns, 1.0)(_ * _)

  // Ex 3.9
  def lengthFoldRight(l: List[Int]): Int = foldRight(l, 0)((_, y) => y + 1)

  // Ex 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Ex 3.11
  def sumFoldLeft(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def productFoldLeft(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // Ex 3.12
  def reverseWithAppend[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(reverseWithAppend(xs), List(x))
  }

  def reverse[A](l: List[A]): List[A] = {
    def iter(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, xs) => iter(xs, Cons(x, acc))
    }
    iter(l, List())
  }

  def reverseFold[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((xs, x) => Cons(x, xs))

  // Ex 3.13
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) =>
      b =>
        g(f(b, a)))(z)

  // Ex 3.14
  def appendFoldR[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((x, xs) => Cons(x, xs))

  def appendFoldL[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((xs, x) => Cons(x, xs))

  // Ex 3.16
  def transform(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, transform(xs))
  }
  def transformFoldR(l : List[Int]) : List[Int] =
    foldRight[Int, List[Int]](l, Nil)((x, xs) => Cons(x + 1, xs))
  def transformFoldL(l : List[Int]) : List[Int] =
    foldLeft[Int, List[Int]](l, Nil)((xs, x) => Cons(x + 1, xs))

  // Ex 3.17
  def map(l : List[Double]) : List[String] =
    foldRight[Double, List[String]](l, List())((x, xs) => Cons(x.toString, xs))
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

val tailRez = List.tail(List(1, 2, 3, 4))
val setHeadRez = List.setHead(List(1, 2, 3, 4), 5)
val dropRez = List.drop(List(1, 2, 3, 4), 0)

// Rez Ex 3.5
val dropWhileRez = List.dropWhile[Int](List(1, 2, 3, 4))(a => a <= 2)

// Rez Ex 3.6
val initRez = List.init(List(1, 2, 3, 4))

val foldRightRez = List.foldRight(List(1, 2, 3, 4, 5), 0)(_ + _)
val sumFoldRightRez = List.sumFoldRight(List(1, 2, 3, 4))

// Rez Ex 3.7
val prodFoldRightRez = List.productFoldRight(List(1, 2, 3, 4))

// Rez Ex 3.9
val lengthFoldRightRez = List.lengthFoldRight(List(1, 2, 3, 4, 5))

// Rez Ex 3.10
val foldLeftRez = List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _)

// Rez Ex 3.11
val sumFoldLeftRez = List.sumFoldLeft(List(1, 2, 3, 4))
val prodFoldLeftRez = List.productFoldLeft(List(1, 2, 3, 4))
val lengthFoldLeftRez = List.lengthFoldLeft(List(1, 2, 3, 4, 5))

// Rez Ex 3.12
val reverseWithAppendRez = List.reverseWithAppend(List(1, 2, 3, 4, 5))
val reverseRez = List.reverse(List(1, 2, 3, 4, 5))
val reverseFoldRez = List.reverseFold(List(1, 2, 3, 4, 5))

// Rez Ex 3.13
val foldLeftViaFoldRightRez = List.foldLeftViaFoldRight(List(1, 2, 3, 4, 5), 0)(_ + _)

// Rez Ex 3.14
val appendFoldLRez = List.appendFoldL(List(1, 2, 3, 4), List(6, 7, 8))
val appendFoldRRez = List.appendFoldR(List(1, 2, 3, 4), List(6, 7, 8))

// Rez Ex 3.16
val transformRez = List.transform(List(1,2,3,4,5))
val transformFoldRRez = List.transformFoldR(List(1,2,3,4,5))

// Rex Ex 3.17
val mapRez = List.map(List(1.0, 2.0 , 3.0, 4.0))