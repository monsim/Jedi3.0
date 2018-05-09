package expression
import context._
import value._

case class Assignment(var vbl: Identifier, var update: Expression) extends SpecialForm{
  //println("hi")
  def execute(env: Environment) = {
   // println("here")
    val v1 = vbl.execute(env)
    //don't modify the environment 
    if (v1.isInstanceOf[Variable]){
      v1.asInstanceOf[Variable].content = update.execute(env)
    } else {
      throw new TypeException("must return a variable")
    }
    Notification.DONE
  }
}