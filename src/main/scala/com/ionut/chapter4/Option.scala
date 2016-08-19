package main.scala.com.ionut.chapter4

import scala.{Option => _}

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse (ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  // def variance(xs: Seq[Double]): Option[Double] =
}

object Test {

  def main(args: Array[String]): Unit = {

    val some = Some(10)
    val none = None

    // Rez ex 4.1
    println("Example for map()")
    println(Some(10).map(_ + 1))
    println(None.map((b: Nothing) => b == null))
    println("Example for map()")

    println("Example for getOrElse()")
    println(Some(10).getOrElse(50))
    println(None.getOrElse(50))
    println("Example for getOrElse()")

    println("Example for flatMap(")
    println(Some(10).flatMap((b: Int) => Some(b + 30)))
    println(None.flatMap((b: Int) => Some(b + 30)))
    println("Example for flatMap()")

    println("Example for orElse()")
    println(Some(10).orElse(Some(50)))
    println(None.orElse(Some(50)))
    println("Example for orElse()")

    println("Example for filter()")
    println(Some(10).filter(_ > 5))
    println(None.filter(_ => true))
    println("Example for filter()")
  }
}
