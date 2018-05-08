package value

import collection.mutable._
import context._

class Store(private var elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value {
  // adds e to the end of store
  def add(e: Value) {elems += e}
  // inserts e at position pos in this
  def put(e: Value, pos: Integer) { elems.insert(pos.value, e) }
  // removes element at position pos from this
  def rem(pos: Integer) { elems.remove(pos.value) }
  // returns element at position pos in this
  def get(pos: Integer): Value = elems(pos.value)
  // returns true ie this contains e
  def contains(e: Value): Boole = Boole(elems.contains(e))
  // returns the size of this
  def size: Integer = Integer(elems.size)
  // returns "{e0 e1 e2 ...}"
  override def toString = {
//    var temp = elems.toString.replaceAll(",", "")
//    temp = temp.replaceFirst("(", "{")
//    temp = temp.replaceAll
    var temp = "{"
    for (i <- elems) {
      temp = temp + i.toString()
    }
    temp  
  }
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {
    //trans.apply(elems.toList) returns a value though
    val list = elems.toList.map((x:Value) => trans(x))  //closure takes params, body, defEnv
    val b = new ArrayBuffer
    for (i <- list){
      b += i
    }
    new Store(b)
  }
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {???}
}