package expression

import context._
import value._

//disjunction ::= expression ~ ("||" ~ expression)*

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  
  def execute(env: Environment):Value = {
    var isTrue = false
    var index = 0
    while (!isTrue && index < operands.length){
      if (operands(index).execute(env).isInstanceOf[Boole]) {
        //if true, then always true, so return true
        if (operands(index).execute(env) == Boole(true)) isTrue = true
        index = index + 1
      } else {
        throw new TypeException
      }
    }
    Boole(isTrue)
  }
}