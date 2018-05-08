package value

import context._
import expression._

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {
  def +(other: Real) = Real(this.value + other.value)
  // *, -, /
  def *(other: Real) = Real(this.value * other.value)
  def -(other: Real) = Real(this.value - other.value)
  def /(other: Real) = {
    other.value match{
      case 0.0 => throw new JediException("cannot divide by 0")
      case _ => Real(this.value / other.value)
    }
  }
  def unary_- = -value // unary negation. check this
  
  //def ==(other: Real) = this.value == other.value
 // override def <(other: Real) = this.value < other.value

  override def toString = value.toString
  def compare(other: Real): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Real]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Real => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##

}

//object Real {
//  def apply(v: Double) = new Real(v)
//}