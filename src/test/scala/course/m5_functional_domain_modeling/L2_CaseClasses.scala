package course.m5_functional_domain_modeling

import course.Lesson

import zio.test._
import zio.test.TestAspect.ignore

//  ██████╗ █████╗ ███████╗███████╗     ██████╗██╗      █████╗ ███████╗███████╗███████╗███████╗
// ██╔════╝██╔══██╗██╔════╝██╔════╝    ██╔════╝██║     ██╔══██╗██╔════╝██╔════╝██╔════╝██╔════╝
// ██║     ███████║███████╗█████╗      ██║     ██║     ███████║███████╗███████╗█████╗  ███████╗
// ██║     ██╔══██║╚════██║██╔══╝      ██║     ██║     ██╔══██║╚════██║╚════██║██╔══╝  ╚════██║
// ╚██████╗██║  ██║███████║███████╗    ╚██████╗███████╗██║  ██║███████║███████║███████╗███████║
//  ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝     ╚═════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝

object L2_CaseClasses extends Lesson {

  /** ✏ EXERCISE
    *
    * Create a Person case class to get free getters (fields) for all the
    * constructor parameters of the class.
    */
  val testFields =
    test("fields") {
      case class Person(name: String, age: Int) // <- Convert this to a case class

      def getName(person: Person): String = person.name // <- Complete these
      def getAge(person: Person): Int     = person.age // <- Complete these

      val holmes = new Person("Sherlock Holmes", 42)

      assertTrue(
        getName(holmes) == "Sherlock Holmes",
        getAge(holmes) == 42
      )
    }

  /** ✏ EXERCISE
    *
    * Create a Person case class with a name (String) and an age (Int), and
    * delete the fake apply constructor to observe the free constructor that all
    * case classes receive in their companion objects.
    */
  val testApply =
    test("apply") {
      case class Person(name: String, age: Int) // <- Convert this to a case class

      object Person {
        def apply(name: String, age: Int) = new Person(name, age)
      }

      assertTrue(Person("Sherlock Holmes", 42) == Person("Sherlock Holmes", 42))
    }

  /** ✏ EXERCISE
    *
    * Get a free implementation of equality for the `Profile` class by turning
    * it into a case class.
    */
  val testEquals = test("equals") {
   case class Profile(val age: Int) // <- Convert this to a case class

    assertTrue(Profile(42) == Profile(42))
  }

  /** ✏ EXERCISE
    *
    * Get a free implementation of hash code for the `CreditCard` class by
    * turning it into a case class.
    */
  val testHashCode =
    test("hashCode") {
      case class CreditCard(val number: String) // <- Convert this to a case class

      assertTrue( CreditCard("123").hashCode == CreditCard("123").hashCode)
    }

  /** ✏ EXERCISE
    *
    * Get a free implementation of `toString` for the `Address` class by turning
    * it into a case class.
    */
  val testToString =
    test("toString") {
      case class Address(val street: String) // <- Convert this to a case class

      assertTrue( Address("221B Baker Street").toString == "Address(221B Baker Street)")
    }

  /** ✏ EXERCISE
    *
    * Get a free implementation of `copy` for the `Permissions` class by turning
    * it into a case class.
    */
  val testCopy =
    test("copy") {
      case class Permissions(canRead: Boolean, canWrite: Boolean, canShare: Boolean)

      val perms = new Permissions(true, false, false)

      assertTrue(perms.copy(canRead = false) == new Permissions(false, false, false))
    }

  def exercise = suite("Case Classes")(
    testFields,
    testApply,
    testEquals,
    testHashCode,
    testToString,
    testCopy
  )

}
