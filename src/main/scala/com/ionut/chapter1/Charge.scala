package com.ionut.chapter1

case class Charge(cc: CreditCard, amount : BigDecimal) {

  def combine(other : Charge) = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("can't combine charge to different cards")
  }
}
