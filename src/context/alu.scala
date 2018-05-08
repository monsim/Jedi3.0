package context


  
import expression._
import value._

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) //binary
      case "more" => more(args) // binary
      case "equals" => equals(args) // note: equals(7, true) = false, not error
      case "unequals" => unequals(args) // binary, = not(equals(args))?
      case "not" => not(args) // unary
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
    private def toInt(arg: Value): Option[Integer] =
      if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
      
    private def toReal(arg: Value): Option[Real] =
      if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
      else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
      else None
      
    private def toChars(arg: Value): Option[Chars] =
      if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None
      
    private def add(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_+_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_+_)
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) args4.flatten.reduce(_+_)
          else {
            throw new TypeException("Inputs to + must be numbers or texts")
          }
        }
      }
    }
  
    
    private def mul(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_*_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_*_)
        else {
          throw new TypeException("Inputs to * must be numbers")
        }
      }
    }
    
    
    private def sub(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_-_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_-_)
        else {
          throw new TypeException("Inputs to - must be numbers")
        }
      }
    }
    
    
    private def div(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_/_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_/_)
        else {
          throw new TypeException("Inputs to / must be numbers")
        }
      }
    }
    
    
  def less(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("less expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) < args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) < args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) < args4(1))
          else throw new TypeException("Inputs to < must be numbers or texts")
        }
      }
   }  
  

   def more(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("more expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) > args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) > args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) > args4(1))
          else throw new TypeException("Inputs to > must be numbers or texts")
        }
      }
   }
  
   
   def equals(args: List[Value]): Value = {  //CANNOT ASSUME TWO ELEMENTS 
     
     if (args.length < 2 || args == Nil) throw new JediException("equals must have at least two parameters")
     else {
       def helper(head: Value, tail: List[Value], result: Boolean): Boolean = {
         if (result) {
           if (tail == Nil) result
           else if (head == tail.head) helper(head, tail.tail, result)
           else helper(head, tail.tail, false)
         } else {
           result
         }
       }
       Boole(helper(args.head, args.tail, true))
     }
   }
   
   def unequals(args: List[Value]): Value = {  //assuming only two elements CHANGE IT???? alu.not(alu.equals([a,b]))
     /*
     if (args.length != 2) throw new JediException("unequals must have two parameters")
     try {
       if (!args(0).equals(args(1))) Boole(true)
       else Boole(false)
     } catch {
       case e => Boole(true)  //return true if types are different
     }
     */
     val equals = alu.equals(args)
     val list: List[Value] = List(equals)
     alu.not(list)  //make a list of equals and then pass to not 
   }
   
   def not(args: List[Value]): Value = {  //assuming only one element
      if (args.length != 1) throw new JediException("unequals must have only one parameter")
      else {
        if (!args(0).isInstanceOf[Boole]) throw new JediException("must be instance of Boole")
        else if (args(0) == Boole(true)) Boole(false)
        else Boole(true)
      }
   }
 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE } 
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
   
   //jedi 3.0 additions 
  // variable ops
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {???}
   
   // creates a new variable cobtaining args(0)
   private def makeVar(args: List[Value]) = {???}
   
   // store ops
   
   // returns a new store containing args
   private def store(args: List[Value]) = {???}
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = {???}
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = {???}
   
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = {???} 
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = {???} 
   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = {???}
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = {???}
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = {???}
   
  // etc.
}