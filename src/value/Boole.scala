package value

import expression._


case class Boole (val value: Boolean) extends Literal {
  def &&(other: Boole) = Boole(value && other.value)
  def ||(other: Boole) = Boole(value || other.value)
  def unary_! = Boole(!value)
  override def toString = value.toString
}