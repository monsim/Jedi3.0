package expression

import context._
import value._

/*
 * from notes
 * lambda is a special form has a list of identifiers for it’s params. 
 * and has an expression body. 
 * has an execute method, produces a closure (new class, extends value)
 */

case class Lambda(val param: List[Identifier], body: Expression) extends SpecialForm{
  /*
   * return a new closure (params of closure are params for lambda), 
   * body for closure is body of lambda, env is the env that is passed in
   */
  def execute(env: Environment): Value = {
    new Closure(param, body, env)
    //Notification.OK
  }
}