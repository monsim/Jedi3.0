package expression

import context._
import value._

//declaration ::= "def" ~ identifier ~ "=" ~ expression

case class Declaration(val id: Identifier, val ex: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    env.put(id, ex.execute(env))
    Notification.OK
  }
}