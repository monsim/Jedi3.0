package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^{
    case id ~ "=" ~ exp => new Assignment(id, exp)
  }
  
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^{
    case "while" ~ "(" ~ exp ~ ")" ~ exp1 => new Iteration(exp, exp1)
  }
  
  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~ expression ~ "]" ^^{ //should it return an expression?
    case "[" ~ exp ~ "]" => Expression(exp)
  }
  

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}