package course.m5_functional_domain_modeling

import course.Lesson
import zio.test._
import zio.test.TestAspect.ignore

// ██████╗  █████╗ ████████╗████████╗███████╗██████╗ ███╗   ██╗    ███╗   ███╗ █████╗ ████████╗ ██████╗██╗  ██╗██╗███╗   ██╗ ██████╗
// ██╔══██╗██╔══██╗╚══██╔══╝╚══██╔══╝██╔════╝██╔══██╗████╗  ██║    ████╗ ████║██╔══██╗╚══██╔══╝██╔════╝██║  ██║██║████╗  ██║██╔════╝
// ██████╔╝███████║   ██║      ██║   █████╗  ██████╔╝██╔██╗ ██║    ██╔████╔██║███████║   ██║   ██║     ███████║██║██╔██╗ ██║██║  ███╗
// ██╔═══╝ ██╔══██║   ██║      ██║   ██╔══╝  ██╔══██╗██║╚██╗██║    ██║╚██╔╝██║██╔══██║   ██║   ██║     ██╔══██║██║██║╚██╗██║██║   ██║
// ██║     ██║  ██║   ██║      ██║   ███████╗██║  ██║██║ ╚████║    ██║ ╚═╝ ██║██║  ██║   ██║   ╚██████╗██║  ██║██║██║ ╚████║╚██████╔╝
// ╚═╝     ╚═╝  ╚═╝   ╚═╝      ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝    ╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝

object L3_PatternMatching extends Lesson {

  /** ✏ EXERCISE
    *
    * Use pattern matching to extract out the `street` of `Address`.
    */
  val testSimple = test("simple") {
    final case class Address(street: String)

    def extractStreet(address: Address): String = address match {
      case Address(street) => street
    }

    assertTrue(extractStreet(Address("221B Baker")) == "221B Baker")
  }

  /** ✏ EXERCISE
    *
    * Use pattern matching to extract out the `postalCode` of `Address`, using a
    * wildcard to ignore (match any) `street`.
    */
  val testWildcard =
    test("wildcard") {
      final case class Address(street: String, postalCode: String)

      def extractPostalCode(address: Address): String = address match {
        case Address(street, code) => code
      }
      assertTrue(extractPostalCode(Address("221B Baker", "NW1 6XE")) == "NW1 6XE")
    }

  /** ✏ EXERCISE
    *
    * Using pattern matching on a constant, implement the provided function so
    * that it returns true for any street matching "221B Baker", regardless of
    * postal code.
    */
  val testConstant =
    test("constant") {
      final case class Address(street: String, postalCode: String)

      def is221B(address: Address): Boolean = address.street match {
        case "221B Baker" => true
        case _ => false
      }

      assertTrue(
        is221B(Address("221B Baker", "NW1 6XE")),
        !is221B(Address("123 Green Street", "NW1 6XE"))
      )
    }

  /** ✏ EXERCISE
    *
    * Using multiple ordered case clauses in a pattern match, implement the
    * `neighbor` function such that it returns the correct `Deduction` based on
    * the given address.
    */
  val testOrdered =
    test("ordered") {
      final case class Address(number: Int, street: String, postalCode: String)

      sealed trait Deduction

      object Deduction {
        // Holmes lives a 221 Baker Street
        case object Holmes extends Deduction
        // A neighbor lives on Baker Street
        case object Neighbor extends Deduction
        // Otherwise, we just can't say
        case object Unknown extends Deduction
      }

      def neighbor(address: Address): Deduction = address match {
        case Address(221, "Baker", "NW1 6XE") => Deduction.Holmes
        case Address(_, "Baker", "NW1 6XE") => Deduction.Neighbor
        case _ => Deduction.Unknown

      } // <- Implement with pattern matching and multiple ordered cases

      assertTrue(
        neighbor(Address(221, "Baker", "NW1 6XE")) == Deduction.Holmes,
        neighbor(Address(219, "Baker", "NW1 6XE")) == Deduction.Neighbor,
        neighbor(Address(219, "Baker", "NW5 7BF")) == Deduction.Unknown,
        neighbor(Address(238, "Baker", "NW1 6XE")) == Deduction.Neighbor,
        neighbor(Address(221, "Green", "NW1 6XE")) == Deduction.Unknown,
        neighbor(Address(180, "Green", "NW1 6XE")) == Deduction.Unknown
      )
    }

  /** ✏ EXERCISE
    *
    * Using pattern guards, implement the provided function so that it returns
    * true for any street that contains "Baker".
    */
  val testPatternGuards =
    test("pattern guards") {
      final case class Address(street: String, postalCode: String)

      def isBaker(address: Address): Boolean = address match {
        case Address(street, _) if street.contains("Baker") => true
        case _ => false
      }

      assertTrue(isBaker(Address("220 Baker", "NW1 6XE")))
    }

  /** ✏ EXERCISE
    *
    * Using nested patterns, implement the provided function so that it extracts
    * out the postal code of any person.
    */

  val testNested =
    test("nested") {
      final case class Person(name: String, address: Address)
      final case class Address(street: String, postalCode: String)

      def extractPostalCode(person: Person): String = person match {
        case Person(_, Address(_, postalCode)) => postalCode
      }

      val sherlock = Person("Sherlock Holmes", Address("221B Baker", "NW1 6XE"))

      assertTrue(extractPostalCode(sherlock) == "NW1 6XE")
    }

  /** ✏ EXERCISE
    *
    * Using stable identifiers, return true if an address street matches the
    * `sherlockStreet` constant.
    */
  val testStableIdentifierPattern =
    test("stable identifier pattern") {
      final case class Address(number: String, street: String, postalCode: String)

      val sherlockStreet = "Baker"

      def isSherlockStreet(address: Address): Boolean =  address match {
        case Address(street, `sherlockStreet`, _)  => true
        case _ => false
      }

      val address = Address("220", "Baker", "NW1 6XE")

      assertTrue(isSherlockStreet(address))
    }

  /** ✏ EXERCISE
    *
    * Use typed patterns to implement the `scan` method, returning the proper
    * ScanResult for the contents of the given Box.
    */
  val testTypedPattern =
    test("typed pattern") {
      final case class Puppy(name: String, isWearingHat: Boolean)
      final case class Box(contents: Any)

      sealed trait ScanResult
      object ScanResult {
        case object IsString extends ScanResult
        case object IsInt    extends ScanResult
        case object IsPuppy  extends ScanResult
        case object IsOther  extends ScanResult
      }

      def scan(box: Box): ScanResult = box match {
        case Box(_: String) => ScanResult.IsString
        case Box(_: Int) => ScanResult.IsInt
        case Box(_: Puppy) => ScanResult.IsPuppy
        case _ => ScanResult.IsOther
      }

      assertTrue(
        scan(Box("Hello")) == ScanResult.IsString,
        scan(Box("Chocolate!")) == ScanResult.IsString,
        scan(Box(123)) == ScanResult.IsInt,
        scan(Box(true)) == ScanResult.IsOther,
        scan(Box(Puppy("Crumb", isWearingHat = true))) == ScanResult.IsPuppy
      )
    }

  def exercise =
    suite("PatternMatching")(
      testSimple,
      testWildcard,
      testConstant,
      testOrdered,
      testPatternGuards,
      testNested,
      testStableIdentifierPattern,
      testTypedPattern
    )

}
