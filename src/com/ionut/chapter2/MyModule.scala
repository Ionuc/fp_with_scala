package com.ionut.chapter2

object MyModule {

  def abs(n: Int): Int = {
//    if (n < 0) -n
//    else n
def go(n: Int, acc: Int): Int = {
  if (n == 1) 1
  else go(n - 1, acc * n)
}
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n == 1) 1
      else go(n - 1, acc * n)
    }
    go(n, 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
