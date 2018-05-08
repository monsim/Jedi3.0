package value

import context._
import expression._
import scala.collection.mutable.ListBuffer

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value{
  def apply(args: List[Value]) = {
    //  1. tempEnv extends defEnv
    //  2. bulk put params and args into tempEnv
    //  3. execute body in tempEnv
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
   }
}

