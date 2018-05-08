package expression
import context._
import value._

class Assignment(var vbl: Identifier, var update: Expression) extends SpecialForm{
  def execute(env: Environment) = {
    val v1 = vbl.execute(env)
    val v2 = update.execute(env)
    //don't modify the environment 
    if (v1.isInstanceOf[Variable]){
      v1.asInstanceOf[Variable].content = v2
    } else {
      throw new TypeException("must return a variable")
    }
    Notification.DONE
  }
}