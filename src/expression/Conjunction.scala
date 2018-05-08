package expression

import context._
import value._

//conjunction ::= expression ~ ("&&" ~ expression)*

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  
  def execute(env: Environment):Value = {
    var isTrue = true
    var index = 0
    while (isTrue && index < operands.length){
      if (operands(index).execute(env).isInstanceOf[Boole]) {
        //if false, always false. return false 
        if (operands(index).execute(env) == Boole(false)) isTrue = false
        index = index + 1
      } else {
        throw new TypeException
      }
    }
    Boole(isTrue)
  }
}