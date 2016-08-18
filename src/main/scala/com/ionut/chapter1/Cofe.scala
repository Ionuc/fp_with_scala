package com.ionut.chapter1

object Cofe {

  def buyCoffeeWithSides(cc: CreditCard) : Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price)
    cup
  }

  def buyCoffee(cc: CreditCard) : (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, new Charge(cc, cup.price))
  }

  def buyCoffees(cc : CreditCard, n: Int) : (List[Coffee], Charge) = {
    val purchases : List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}
