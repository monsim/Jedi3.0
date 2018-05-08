package expression
import context._
import value._

class Iteration(var condition: Expression, var body: Expression) extends SpecialForm{
  def execute(env: Environment) = {  //CHECK TYPE
    while (condition.execute(env) == Boole(true))
      body.execute(env)
    Notification.DONE
  }
}