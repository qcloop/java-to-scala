package course.m8_patterns

import course.Lesson
import course.Lesson.???
import zio.test.TestAspect.ignore
import zio.test.assertTrue

object L2_MonoidInstances extends Lesson {

  trait Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  def smoosh[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.combine)

  /** ✏ EXERCISE
    *
    * Create a monoid for the `String` type, which will concatenate Strings
    * together.
    */

  // CREATE THIS INSTANCE
  implicit lazy val stringMonoid: Monoid[String] =
    ???

  val stringTest =
    test("String Monoid") {
      val values = List("a", "b", "c", "d")
      assertTrue(smoosh(values) == "abcd")
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a monoid for the `Multiply` type, which will multiply Int values
    * together.
    */
  final case class Multiply(int: Int)

  object Multiply {
    // CREATE THIS INSTANCE
    implicit val monoid: Monoid[Multiply] =
      ???
  }

  val multiplyTest =
    test("Multiply Monoid") {
      val values = List(Multiply(1), Multiply(2), Multiply(3), Multiply(4))
      assertTrue(smoosh(values) == Multiply(24))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a monoid for the `Add` type, which will add Int values together.
    */
  final case class Add(int: Int)

  object Add {
    // CREATE THIS INSTANCE
    implicit val monoid: Monoid[Add] =
      ???
  }

  val addTest =
    test("Add Monoid") {
      val values = List(Add(1), Add(2), Add(3), Add(4))
      assertTrue(smoosh(values) == Add(10))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * ADVANCED
    *
    * Create a monoid for the `Map[K, V]` type, which will concatenate maps,
    * given that their values are monoids.
    */
  implicit def mapMonoid[K, V]: Monoid[Map[K, V]] = ???

  val mapTest =
    test("Map Monoid") {
      val values =
        List(
          Map("a" -> Multiply(2)),
          Map("b" -> Multiply(8)),
          Map("a" -> Multiply(4)),
          Map("b" -> Multiply(3)),
          Map("c" -> Multiply(10))
        )

      assertTrue(smoosh(values) == Map("a" -> Multiply(8), "b" -> Multiply(24), "c" -> Multiply(10)))
    } @@ ignore

  override def exercise =
    suite("Monoid")(
      stringTest,
      multiplyTest,
      addTest,
      mapTest
    )
}
