package context

import scala.collection.mutable._
import value._
import expression._

class Environment(var extension: Environment = null)
   extends HashMap[Identifier, Value] with Value {

  // used by closures to bind parameters to arguments
  def bulkPut(params: List[Identifier], args: List[Value]) {
    if (params.length != args.length) throw new TypeException("# arguments != #parameters")
    for(i <- 0 until params.length) this.put(params(i), args(i))
  }
  
  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw new UndefinedException(name)
  }
  

  override def contains(name: Identifier): Boolean = {
    super.contains(name)
  }
  
  /*
  override def contains(name: Identifier): Boolean = { 
    try {
      apply(name)
      true
    } catch {
      case e: UndefinedException => false
    }
  }
  */
  
  /*
  override def contains(name: Identifier): Boolean = { 
    def helper(env: Environment): Boolean = {
      if (env.hasName(name)) true    //use apply?
      else if (env.extension != null) helper(env.extension)
      else false
    }
    helper(this)
  }
  */
  def hasName(name: Identifier): Boolean = {
    var toReturn = false
    for(i <- this.keys){
      if (i.equals(name)) toReturn = true
    }
    toReturn
  }
  
  
}