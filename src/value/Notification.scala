package value

class Notification(val message: String) extends Value {
  override def toString: String = message;
}
  
object Notification extends Value {
  def apply(m: String) = new Notification(m) 
  //val OK = "ok"
  //val DONE = "done"
  //val UNSPECIFIED = "unspecified"
  def OK = Notification("ok")
  def DONE = Notification("done")
  def UNSPECIFIED = Notification("unspecified")
}