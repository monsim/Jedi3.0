package expression

import context._
import value._

//ORDER OF CONDITION, CONSEQUENT, ALTERNATIVE??

 //conditional ::= "if" ~ "(" ~ expression ~ ")" ~ expression ~ ("else" ~ expression)?
case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm{
  
  def execute(env: Environment):Value = {
    if (condition.execute(env).isInstanceOf[Boole]){
      if(condition.execute(env) == Boole(true)){
        consequent.execute(env)
      } else {
        if (alternative != null)
          alternative.execute(env)
        else 
          Notification.UNSPECIFIED
      }
    } else {
      throw new TypeException
    }
  }
}