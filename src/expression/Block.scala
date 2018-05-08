package expression


import context._
import value._

//block ::= {exp1; exp2; ...; expn}

case class Block(val exp: List[Expression]) extends SpecialForm{
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)  //create temp env that extends the current env
    for (i <- 0 to exp.length-2){
      exp(i).execute(tempEnv)
    }
    exp(exp.length-1).execute(tempEnv)  //return last one execute
  }
}