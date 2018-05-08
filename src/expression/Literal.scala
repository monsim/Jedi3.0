package expression

import context._
import value._


trait Literal extends Expression with Value {
  def execute(env: Environment) = {
     this
   } 
}