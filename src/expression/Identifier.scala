package expression


import context._

case class Identifier(val name: String) extends Expression {
   override def toString = name
   
   def execute(env: Environment) = {
     env(this)
   }
}

