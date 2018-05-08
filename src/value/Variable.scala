package value

case class Variable(var content: Value) extends Value{
  override def toString() = {
    "[" + content.toString + "]"
  }
}