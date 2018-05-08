package value


import expression._

case class Chars (val value: String) extends Literal with Ordered[Chars] with Equals {
  def +(other: Chars) = {
    println(Notification.OK)
    Chars(value + other.value)
  } 
  //def <(other: Chars) = value < other.value
  //def ==(other: Chars) = value.equals(other.value)
  def substring(start: Integer, end: Integer) = {
    Chars(value.substring(start.value, end.value))
  }
  override def toString = value
  def canEqual(other: Any) =  other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Chars => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  def compare(other: Chars): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  
   override def hashCode = this.toString.##
}

//object Chars extends Literal {
//  def apply(m: String) = new Chars(m)
//}