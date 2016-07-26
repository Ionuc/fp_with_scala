package com.ionut.chapter1


class Chapter1 {
  def main(args:Array[String]) = {
    var greeting = ""
    for (i <- 0 until 2) {
      greeting += (2 + " ")
    }
    if (args.length > 0) greeting = greeting.substring(0, greeting.length - 1)

    println(greeting)
  }
}
