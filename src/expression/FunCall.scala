package expression

import context._
import value._
import scala.collection.mutable.ListBuffer

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  def execute(env: Environment): Value = {
    
    /*
     * jedi 1.0
     var arguments = ListBuffer[Value]()
     for (i <- operands) {
       arguments += i.execute(env)
     }
     alu.execute(operator, arguments.toList)
     * 
     */ 
    
    /*
     //environment contains method
    val args = operands.map(_.execute(env))  //execute operands to get arguments
    if (env.contains(operator)){
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) maybeClosure.asInstanceOf[Closure].apply(args)
      else throw new TypeException("must be a closure")
    } else {
      alu.execute(operator, args)
    }
    */
    
    
    
     // try catch method 
    val args = operands.map(_.execute(env))
	  try {
		  val maybeClosure = operator.execute(env)
			if (!maybeClosure.isInstanceOf[Closure]) throw new TypeException("needs to be a closure")
			else maybeClosure.asInstanceOf[Closure].apply(args)
		} catch {
			case e: UndefinedException => alu.execute(operator, args)
		}
		
  }
}