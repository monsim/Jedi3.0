
package test

import value._

object NumberTest1 extends App {
  var i1 = Integer(7)
  var i2 = Integer(6)
  println("i1 * i2 = " + (i1 * i2))
  println("i1 == i2 = " + (i1 == i2))
  println("i1 < i2 = " + (i1 < i2))
  println("i1.## = " + i1.##)
  
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("-r1 = " + -r1)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
  println("r1.## = " + r1.##)
  
  println("r1 * i2 = " + (r1 * i2))
  println("i1 * r2 = " + (i1 * r2))
}


/*
 * prints

i1 * i2 = 42
i1 == i2 = false
i1 < i2 = false
i1.## = 55
-r1 = -3.14
r1 * r2 = 8.5094
r1 == r2 = false
r1 < r2 = false
r1.## = 1565118
r1 * i2 = 18.84
i1 * r2 = 18.97
 */

