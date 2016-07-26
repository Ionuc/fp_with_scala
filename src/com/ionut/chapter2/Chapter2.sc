object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n == 1) acc
      else go(n - 1, acc * n)
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    if (n < 1) 0
    else if (n == 1) 0
    else if (n == 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  def findFirst[A](ss: Array[A], key: A, f: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def loop(ss: Array[A], index: Int): Int = {
      if (index >= ss.length) -1
      else if (f(ss.apply(index), key)) index
      else loop(ss, index + 1)
    }
    loop(ss, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(as: Array[A], i: Int): Boolean = {
      if (i >= as.length) true
      else if (!ordered.apply(as.apply(i - 1), as.apply(i)))
        false
      else loop(as, i + 1)
    }
    if (as.length <= 1) true
    else loop(as, 1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)


  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult(" absolute value is ", -42, abs))
    println(formatResult(" factorial value is ", 7, factorial))
  }

}

MyModule.abs(5)

MyModule.factorial(5)
MyModule.fib(8)

MyModule.main(Array())

MyModule.findFirst[String](
  Array.apply("ion", "marie"), "bla",
  (v1, v2) => v1.equals(v2))

MyModule.isSorted[Int](
  Array.apply(1, 2, 3, 4, 2, 6, 7),
  (v1, v2) => v1 < v2)

val partial1Rez1 = MyModule.partial1[Int, Int, Int](
  1,
  (b, c) => b * c)
val partial1Rez2 = partial1Rez1(2)

val curryRez1 = MyModule.curry[Int, Int, Int]((a, b) => a * b)
val curryRez2 = curryRez1(1)
val curryRez3 = curryRez2(2)

val uncurryRez1 = MyModule.uncurry[Int, Int, Int](
  a => b => a + b
)
val uncurryRez2 = uncurryRez1(1, 2)

val composeRez1 = MyModule.compose[Int, Int, Int](
  n1 => n1 + 1,
  n2 => n2 * n2
)
val composeRez2 = composeRez1(2)