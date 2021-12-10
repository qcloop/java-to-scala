package course.m1_basics

// - The main method
// - Defining methods
// - Expressions vs. Statements
// - val vs. var
// - Objects
// - Purity
// - Functions as Values

object Counter {
  var value: Int = 0

  def increment = {
    value += 1
    value
  }
}
object Example extends App {

  def foo(x: Int) : Unit =
    println("foo is called with arg = " + x)

  foo(10)
  foo(11)
  println("Counter increment " + Counter.increment)
  println("Counter increment " + Counter.increment)
  println("Counter increment " + Counter.increment)
}