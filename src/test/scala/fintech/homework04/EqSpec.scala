package fintech.homework04

import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.{ComplexNumber, Eq}
import fintech.homework06.Eq._
import fintech.homework06.Scale

class EqSpec extends FlatSpec with Matchers {

  "Eq" should "work correct with list" in {
    Seq(1) ==== Seq(2) should be(false)
    Seq(1, 2) ==== Seq(1, 2) should be(true)
  }

  "Eq" should "work correct with map" in {
    Map(1 -> 1) ==== Map(1 -> 1) should be(true)
    Map(1 -> 1) ==== Map(1 -> 2) should be(false)
    Map("1" -> 1) ==== Map("1" -> 2) should be(false)
  }

  "Eq" should "work correct with complex numbers" in {
    implicit val sc: Scale = Scale(2)
    new ComplexNumber(1.5, 1.0) ==== new ComplexNumber(1.0, 2.0) should be(false)
    new ComplexNumber(1, 2) ==== new ComplexNumber(1, 2) should be(true)
    new ComplexNumber(1.12, 2) ==== new ComplexNumber(1.123, 2) should be(true)
    new ComplexNumber(1.11, 2) ==== new ComplexNumber(1.123, 2) should be(false)
  }

  "Eq" should "work correct with Option" in {
    Option(1) ==== Option(2) should be(false)
    Option(1) ==== Option(1) should be(true)
    val eq = Option(Seq(new ComplexNumber(1, 2))) ==== Option(Seq(new ComplexNumber(1, 2)))
    eq should be(true)
  }
}
