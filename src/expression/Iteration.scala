package expression
import context._
import value._

case class Iteration(var condition: Expression, var body: Expression) extends SpecialForm{
  def execute(env: Environment) = {  //CHECK TYPE
    if (condition.execute(env).isInstanceOf[Boole]) {
      while (condition.execute(env).asInstanceOf[Boole] == Boole(true))
        body.execute(env)
      Notification.DONE
    } else throw new TypeException("The condition must be a Boole")
  }
}