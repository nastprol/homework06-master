package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '====', деривацию для Map Seq Option
Опционально - разработать ==== для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {

  implicit def seqEq[A](implicit a: Eq[A]): Eq[Seq[A]] = {
    (lft, rgt) => {
      lft.zip(rgt).forall(values => a.equiv(values._1, values._2))
    }
  }

  implicit def mapEq[A, B](implicit a: Eq[A], b: Eq[B]): Eq[Map[A, B]] = {
    (lft, rgt) => {
      lft.keys.zip(rgt.keys).forall(keys => a.equiv(keys._1, keys._2)) &&
        lft.values.zip(rgt.values).forall(values => b.equiv(values._1, values._2))
    }
  }

  implicit def optionEq[A](implicit a: Eq[A]): Eq[Option[A]] = {
    (lft: Option[A], rgt: Option[A]) => a.equiv(lft.get, rgt.get)
  }

  implicit val intEq: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt
  implicit val doubleEq: Eq[Double] = (lft: Double, rgt: Double) => lft == rgt
  implicit val strEq: Eq[String] = (lft: String, rgt: String) => lft == rgt

  implicit def complexEq(implicit eq: Eq[Double], sc: Scale = Scale(120)): Eq[ComplexNumber] = {
    (lft: ComplexNumber, rgt: ComplexNumber) => {
      val lr = BigDecimal(lft.real).setScale(sc.value, BigDecimal.RoundingMode.DOWN).toDouble
      val li = BigDecimal(lft.image).setScale(sc.value, BigDecimal.RoundingMode.DOWN).toDouble
      val rr = BigDecimal(rgt.real).setScale(sc.value, BigDecimal.RoundingMode.DOWN).toDouble
      val ri = BigDecimal(rgt.image).setScale(sc.value, BigDecimal.RoundingMode.DOWN).toDouble
      eq.equiv(lr, rr) && eq.equiv(li, ri)
    }
  }

  implicit class Equiv[A](l: A) {
    def ====(r: A)(implicit eq: Eq[A]): Boolean = {
      eq.equiv(l, r)
    }
  }

}

case class Scale(value: Int)

class ComplexNumber(val real: Double, val image: Double) {

  def +(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + b.real, image + b.image)
  }

  def *(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real * b.real - image * b.image, image * b.real + real * b.image)
  }

  private def GetModulus(): Double = math.sqrt(real * real + image * image)

  private def GetArgument(): Double = math.atan(image / real)

  def ~(pow: Int): ComplexNumber = {
    val module = GetModulus()
    val arg = GetArgument()
    val moduleInPow = math.pow(module, pow)
    new ComplexNumber(math.cos(pow * arg) * moduleInPow, math.sin(pow * arg) * moduleInPow)
  }

  private def imageToString(): String = math.abs(image) match {
    case 1 => "i"
    case 0 => ""
    case _ => math.abs(image).toString + "i"
  }

  override def toString: String = {
    if (real == 0 && image == 0)
      return "0"
    real match {
      case 0 => (if (image < 0) "-") + imageToString()
      case _ =>
        val sign = if (image < 0) " - " else if (image > 0) " + " else ""
        real.toString + sign + imageToString()
    }
  }

  override def equals(a: Any): Boolean = {
    a match {
      case complex: ComplexNumber => real == complex.real && image == complex.image
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 97
    var hash = 1
    hash = prime * hash + image.hashCode()
    hash = prime * hash + real.hashCode()
    hash
  }

  // Написать класс описывающий комплексные числа.
  // Реализовать проверку на равенство, умножение и сложение, toString.
  // Реализовать оператор возведения в целую степень: "~".
  // Реализовать тесты в ComplexNumberSpec
}
