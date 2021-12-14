package course.m1_basics

import course.Lesson
import zio.test._

/** Tony Hoare famously called nulls the "billion dollar mistake".
  * Realistically, the cost they impose on production software is much higher.
  * Functional Scala does not use null values, choosing instead to reflect
  * optionality with data types that convey information at compile-time. This
  * decision results in no NullPointerException, which in turn results in more
  * reliable applications with better-defined error handling.
  *
  * In this module, you will learn how to replace nulls with options.
  */
object NullBasics extends Lesson {

  /** ✏ EXERCISE
    *
    * The `parentOf` function returns `null` for some paths. Modify the function
    * to return `Option[String]` rather than `File | Null`.
    */
  val applyTest = test("apply") {
    import java.io.File

    def parentOf(file: String): Option[String] = {
      val parent = new File(file).getParent
      if (parent == null) None
      else Some(parent)
    }

    def parentOf2(file: String): Option[String] = Option(new File(file).getParent)

    assertTrue(parentOf("") != null)
    assertTrue(parentOf2("") != null)

  }

  /** ✏ EXERCISE
    *
    * Using the `Some` and `None` constructors of `Option` directly, construct
    * an `Option[A]` from an `A` value that might be null.
    */
  val someNoneTest = test("Some / None") {
    def fromNullable[A](a: A): Option[A] = a match {
      case 0 => None
      case null => None
      case x => Some(x)
    }

    val nullString = null.asInstanceOf[String]

    assertTrue(fromNullable(nullInt) == None
      && fromNullable(42) == Some(42)
      && fromNullable(null) == None)

  }

  /** ✏ EXERCISE
    *
    * Using `Option#getOrElse`, use the `DefaultConfig` fallback if the
    * `loadConfig` method returns `None`.
    */
  val getOrElseTest = test("getOrElse") {
    final case class Config(host: String, port: Int)

    val DefaultConfig = Config("localhost", 12345)

    def loadConfig(): Option[Config] = None

    def config: Config = loadConfig() match {
      case None => DefaultConfig
      case a => a.get
    } // <- EDIT HERE

    assertTrue(config != null)
  }

  /** ✏ EXERCISE
    *
    * Using `Option#map`, convert an `Option[Int]` into an `Option[Char]` by
    * converting the int to a char.
    */
  val mapTest = test("map") {
    val option: Option[Int] = Some(42)

    def convert(o: Option[Int]): Option[Char] = o match {
      case Some(a) => Some(a.toChar)
      case None => None
    }

    assertTrue(convert(option) == Some(42.toChar))
  }

    assertTrue(
      convert(option) == Some(42.toChar),
      convert(Option.empty[Int]) == None
    )
  }

  /** ✏ EXERCISE
    *
    * Implement the function `both`, which can combine two options into a single
    * option with a tuple of both results.
    */
  val bothTest = test("both") {
    def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
      left match {
        case Some(i) => right match {
          case Some(s) => Some((i, s))
          case None => None
        }
        case None => None
      }

    def both2[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
      (left, right) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _ => None
      }


    assertTrue(both(Some(42), Some(24)) == Some((42, 24)))
    assertTrue(both2(Some(42), None) == None)
  }
    

  /** ✏ EXERCISE
    *
    * Implement the function `firstOf`, which can combine two options into a
    * single option by using the first available value.
    */
  val firstOfTest = test("firstOf") {
    def firstOf[A](left: Option[A], right: Option[A]): Option[A] =
      if (!left.isEmpty) left
      else if (!right.isEmpty) right
      else None

    assertTrue(firstOf(None, Some(24)) == Some(24))
  }


  /** ✏ EXERCISE
    *
    * Implement the function `chain`, which will pass the value in an option to
    * the specified callback, which will produce another option that will be
    * returned. If there is no value in the option, then the return value of
    * `chain` will be `None`. Notice the "short-circuiting" behavior of the
    * `chain` method. What else does this remind you of?
    */
  val chainTest = test("chain") {
    def chain[A, B](first: Option[A], andThen: A => Option[B]): Option[B] =
      first match {
        case Some(a) => andThen(a)
        case None => None
      }


    assertTrue(
      chain(Some(42), (x: Int) => if (x < 10) None else Some(x)) == Some(42),
      chain(Some(5), (x: Int) => if (x < 10) None else Some(x)) == None,
      chain(None, (x: Int) => if (x < 10) None else Some(x)) == None
    )
  }

  /** ✏ EXERCISE
    *
    * Using `Option#flatMap`, simplify the following pattern-matching heavy
    * code.
    */
  val flatMapTest = test("flatMap") {
    final case class LatLong(lat: Double, long: Double)
    final case class Location(country: String, latLong: Option[LatLong])
    final case class Profile(location: Option[Location])
    final case class User(name: String, profile: Option[Profile])

    def getLatLong(user: User): Option[LatLong] =
      user.profile match {
        case None => None
        case Some(v) =>
          v.location match {
            case None => None
            case Some(v) => v.latLong
          }
      }

    def getLatLong2(user: User): Option[LatLong] =
      user.profile.flatMap(p => p.location.flatMap(l => l.latLong))


    val latLong = LatLong(123, 123)

    val user = User("Holmes", Some(Profile(Some(Location("UK", Some(latLong))))))

    assertTrue(getLatLong(user) == Some(latLong))

  }

  def exercise =
    suite("basics")(
      applyTest,
      someNoneTest,
      getOrElseTest,
      mapTest,
      bothTest,
      firstOfTest,
      chainTest,
      flatMapTest
    )

}

object MigrateFromNullToOption extends Lesson {

  /** ✏ EXERCISE
    *
    * Create a null-safe version of System.property methods.
    */
  val propertyTest = test("property") {
    object SafeProperty {
      def getProperty(name: String): Option[String] = {
        val value = System.getProperty(name)
        if (value == null) None
        else Some(value)
      }

      def getIntProperty(name: String): Option[Int] = getProperty(name).flatMap(n => try {
        Some(n.toInt)
      } catch {
        case _: NumberFormatException => None
      })

      def getIntProperty2(name: String): Option[Int] = getProperty(name).flatMap(_.toIntOption)

      def getBoolProperty(name: String): Option[Boolean] = getProperty(name).flatMap(n => try {
        Some(n.toBoolean)
      } catch {
        case _: IllegalArgumentException => None
      })
    }

    System.setProperty("number", "888")
    System.setProperty("boolean", "true")


      SafeProperty.getProperty("user.dir").isDefined,
      SafeProperty.getIntProperty2("number").isDefined,
      SafeProperty.getProperty("boolean").isDefined
      SafeProperty.getProperty("user.dir").isDefined
    )
  }

  /** ✏ EXERCISE
    *
    * Rewrite the following code to use `Option` instead of nulls.
    */
  val sherlockTest = test("example 1") {
    final case class Address(street: Option[String])
    final case class Profile(address: Option[Address])
    final case class User(id: String, profile: Option[Profile])

    val profile1 = Profile(None)
    val address = Some(Address(None))
    val profile2 = Profile(address)
    val user1 = Some(User("Sherlock Holmes", None))
    val user2 = Some(User("Sherlock Holmes", Some(profile1)))
    val user3 = Some(User("Sherlock Holmes", Some(profile2)))

    def getStreet(user: Option[User]): Option[String] =
      user.flatMap(u => u.profile.flatMap(p => p.address.flatMap(a => a.street)))

    def getStreet2(user: Option[User]): Option[String] =
      for {
        u  <- user
        p  <- u.profile
        a  <- p.address
        s <- a.street
      } yield s

    def assertFails(value: => Any) = assertTrue(value == None)

    assertFails(getStreet(user1)) &&
      assertFails(getStreet(user2)) &&
      assertFails(getStreet(user3)) && assertFails(getStreet2(user3))
  }

  def exercise =
    suite("migrate from null to option")(
      propertyTest,
      sherlockTest
    )
}
