package course.m5_functional_domain_modeling

import course.Lesson
import course.Lesson.???
import zio.test.TestAspect.ignore
import zio.test._

import scala.annotation.tailrec
import scala.io.StdIn

// ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗███╗   ██╗ ██████╗
// ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║████╗  ██║██╔════╝
// ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║██╔██╗ ██║██║  ███╗
// ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║██║╚██╗██║██║   ██║
// ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗██║██║ ╚████║╚██████╔╝
// ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚═╝╚═╝  ╚═══╝ ╚═════╝

object L5_Modeling extends Lesson {

  /** ✏ EXERCISE
    *
    * Create a precise data model for `RelationshipStatus`, which models the
    * relationship status of an individual: married, single, divorced.
    */
  val testRelationshipStatus =
    test("RelationshipStatus") {
      type RelationshipStatus = ???

      def makeMarried: RelationshipStatus = ???

      def makeSingle: RelationshipStatus = ???

      assertTrue(makeMarried != makeSingle)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a precise data model for an `IceCreamOrder`, which allows a
    * customer to choose one of four ice cream flavors in either a cup, cone or
    * milkshake (Conveyance), with an optional cherry-on-top.
    */
  val testIceCream =
    test("IceCreamOrder") {
      type IceCreamOrder = ???

      type Flavor     = ???
      type Conveyance = ???

      def rockyRoad: Flavor     = ???
      def chocolate: Flavor     = ???
      def milkshake: Conveyance = ???
      def cone: Conveyance      = ???

      def makeOrder(flavor: Flavor, conveyance: Conveyance, hasCherry: Boolean): IceCreamOrder = ???

      val order1 = makeOrder(chocolate, milkshake, true)
      val order2 = makeOrder(rockyRoad, cone, false)

      assertTrue(
        order1 == order1,
        order1 != order2
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a precise data model for a user's stock portfolio.
    */
  val testPortfolio =
    test("portfolio") {
      final case class Portfolio() { // <- Complete this type
        def add(symbol: Symbol, amount: Double): Portfolio = ???
      }

      object Portfolio {
        val empty: Portfolio = ???
      }

      type Symbol = ??? // <- Complete this type

      def AAPL: Symbol = ???
      def MSFT: Symbol = ???
      def GOOG: Symbol = ???

      val p1 = Portfolio.empty.add(AAPL, 1.0).add(AAPL, 1.0).add(MSFT, 2.0)
      val p2 = Portfolio.empty.add(MSFT, 2.0).add(AAPL, 2.0)
      val p3 = Portfolio.empty.add(GOOG, 9999.0)

      assertTrue(
        p1 == p2,
        p1 != p3
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Migrate this imprecise data model to a precise data model comprised of
    * sealed traits and case classes, replacing nullable fields and throwing
    * methods with type-safe analogues.
    */
  class S3Config(val accessKey: String, val bucket: String)
  class LocalConfig(val path: String)

  class ParsedConfig(
                      configType: String,
                      s3Config: S3Config,
                      localConfig: LocalConfig
                    ) {
    def connect(): Unit =
      if (configType == "s3")
        println(s"Connecting to S3 bucket ${s3Config.bucket}")
      else
        println(s"Connecting to local file system at ${localConfig.path}")
  }

  object ParsedConfig {
    def load(data: Map[String, String]): ParsedConfig =
      data("configType") match {
        case "s3" =>
          val s3Config = new S3Config(data("accessKey"), data("bucket"))
          new ParsedConfig("s3", s3Config, null)
        case "local" =>
          val localConfig = new LocalConfig(data("path"))
          new ParsedConfig("local", null, localConfig)
      }
  }

  val testConfig =
    test("Config") {
      val s3Data = Map(
        "configType" -> "s3",
        "accessKey"  -> "123",
        "bucket"     -> "bucket"
      )

      val localData = Map(
        "configType" -> "local",
        "path"       -> "/tmp/data"
      )

      val wrongData = Map(
        "favoriteMovie" -> "The Matrix",
        "favoriteFood"  -> "Pizza",
        "favoriteColor" -> "Blue"
      )

      //      ** UNCOMMENT THESE LINES **
      //      ===========================
      //      val s3Config: Option[Config] = ParsedConfig.load(s3Data)
      //      val localConfig: Option[Config] = ParsedConfig.load(localData)
      //      val wrongConfig: Option[Config] = ParsedConfig.load(wrongData)

      assertTrue(
        //        ** UNCOMMENT THESE ASSERTIONS **
        //        ================================
        //        s3Config.get == S3Config("123", "bucket"),
        //        localConfig.get == LocalConfig("/tmp/data"),
        //        wrongConfig == None
        true
      )
    } @@ ignore

  def exercise =
    suite("Modeling")(
      testRelationshipStatus,
      testIceCream,
      testPortfolio,
      testConfig
    )
}

/** Functional Core / Imperative Shell
  *
  * The constraints of immutable data forces new program architectures. At
  * first, this can be a challenging, if not stupefying. However, these newer,
  * more limited techniques tend to lead to better and more consistent designs.
  *
  * The most common high-level functional architecture is often referred to
  * Functional Core / Imperative Shell. This can be implemented in any language,
  * yet it truly shines in Scala.
  *
  * With Functional Core / Imperative Shell, we try to design as much of our
  * logic as possible with immutable data. In Scala, this Functional Core would
  * consist of ADTs formed of sealed trait and case class hierarchies. This
  * purely functional state is then wrapped in a thin layer of mutable code, the
  * Imperative Shell, which keeps a mutable reference to the immutable state and
  * provides an interface to the messy world at the boundaries: e.g., User
  * input, web requests, database access, etc.
  *
  * Exercise
  *
  * Your task is to complete the following Counter command-line application,
  * which is using the Functional Core / Imperative Shell design.
  */
object FunctionalCounter {

  /** The Functional Core.
    *
    * This is comprised of immutable data, which contains pure methods that
    * generate subsequent states based upon input.
    */
  final case class State(count: Int) {

    /** Process the action and create a modified copy which represents the next
      * iteration of the State.
      */
    def process(action: Action): State = ???

    /** Use the current state to render a user-readable string that will be
      * printed to the console.
      */
    def render: String = ???
  }

  object State {
    def empty: State = State(0)
  }

  sealed trait Action
  object Action {
    case object Add      extends Action
    case object Subtract extends Action
    case object Reset    extends Action

    def fromString(string: String): ProcessResult = string match {
      case "+"     => ProcessResult.succeed(Add)
      case "-"     => ProcessResult.succeed(Subtract)
      case "reset" => ProcessResult.succeed(Reset)
      case _       => ProcessResult.fail
    }
  }

  /** A monomorphic version of Option[Action]
    */
  sealed trait ProcessResult
  object ProcessResult {
    def succeed(action: Action): ProcessResult = Succeed(action)
    def fail: ProcessResult                    = Fail

    final case class Succeed(action: Action) extends ProcessResult
    final case object Fail                   extends ProcessResult
  }
  import ProcessResult._

  /** BONUS
    *
    * Rewrite the game loop into a tail-recursive function, removing any
    * explicit mutability.
    */
  def gameLoop(): Unit = {
    var state = State.empty
    var loop  = true
    while (loop) {
      println(state.render)
      print("> ")
      Action.fromString(StdIn.readLine()) match {
        case Succeed(action) => state = state.process(action)
        case Fail            => loop = false
      }
    }
  }

  def main(args: Array[String]): Unit = gameLoop()

}

/** PROJECT
  *
  * Using the Functional Core / Imperative Shell paradigm, implement a game of
  * TicTacToe. Focus first on designing the immutable game state. Once the
  * design of the core has settled, integrate it into the imperative shell—the
  * game loop.
  */

object TicTacToe {

  /** The Functional Core, a purely functional representation of TicTacToe game
    * state.
    *
    * Its methods, based on its input, will generate modified copies of
    * itself—the next iteration of the game state. The methods may also project
    * out some value given the current state, e.g., whether or not the game is
    * still active.
    */

  trait Cell

  object Cell {

  case object Free extends Cell

  case object Player extends Cell

  case object Computer extends Cell

}

  final class Board private (matrix: Array[Array[TicTacToe.Cell]]){
  }

  object Board {
    //  def makeMove(position: Int, owner: Cell) : Board = {

    //  }
  }

  final case class State() {

    /** Should return true if there is not yet a winner or a draw.
      */
    def isActive: Boolean = ???

    def render: String = ???

    def nextState(action: Action): State = ???
  }

  object State {
    val empty: State = ??? // <- Complete this definition
  }

  type Action = ??? // <- Complete this type

  object Action {
    def fromString(string: String): Option[Action] = ???
  }

  /** The Imperative Shell.
    *
    * This deals primarily with user input and updating a mutable reference to
    * the game state.
    */
  def main(args: Array[String]): Unit = {
    @tailrec
    def loop(state: State): Unit = {
      println(state.render)
      print("> ")
      if (state.isActive)
        Action.fromString(StdIn.readLine()) match {
          case Some(action) =>
            loop(state.nextState(action))
          case None =>
            println(s"Invalid input! Try again.")
            loop(state)
        }
    }

    loop(State.empty)
  }
}

/** PROJECT
  *
  * While many programming languages have a construct like case classes, few
  * have the power of sealed traits, and most do not have the pattern matching
  * capabilities of Scala. With the combination of these powerful features, you
  * can construct very precise data models that eliminate runtime errors and
  * make it easier than ever to test and maintain code.
  *
  * In this graduation project, you will gain experience constructing precise
  * data models using case classes and sealed traits.
  */
object DataGraduation {

  sealed trait Command
  object Command {
    case object Exit                        extends Command
    final case class Look(what: String)     extends Command
    final case class Go(where: String)      extends Command
    final case class Take(what: String)     extends Command
    final case class Drop(what: String)     extends Command
    final case class Fight(who: String)     extends Command
    final case class TalkTo(who: String)    extends Command
    final case class Unknown(input: String) extends Command

    def fromString(input: String): Command =
      input.trim.toLowerCase.split("\\w+").toList match {
        case "exit" :: Nil                => Exit
        case "look" :: what :: Nil        => Look(what)
        case "go" :: where :: Nil         => Go(where)
        case "take" :: what :: Nil        => Take(what)
        case "drop" :: what :: Nil        => Drop(what)
        case "fight" :: who :: Nil        => Fight(who)
        case "talk" :: "to" :: who :: Nil => TalkTo(who)
        case _                            => Unknown(input)
      }
  }

  /** ✏ EXERCISE
    *
    * Construct a data model for the state of a game world in a text-based
    * role-playing game. The data model should represent the player character,
    * the map of the game world, items and characters in the game world, and
    * anything else relevant to the game.
    */
  final case class State(playerName: String)
  final case class Step(nextState: Option[State], output: String)

  /** ✏ EXERCISE
    *
    * Implement the `nextStep` function in such a fashion that new states for
    * the game world are constructed from both the old state and the current
    * command read from the user.
    */
  def nextStep(state: State, command: Command): Step = ???

  @tailrec
  def gameLoop(state: State): Unit = {
    println("look | go | tak | drop | fight | talk to")
    print("> ")
    val line                    = StdIn.readLine()
    val command                 = Command.fromString(line)
    val Step(nextState, output) = nextStep(state, command)

    println(output)

    nextState match {
      case Some(state) => gameLoop(state)
      case None        => ()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to the game! What is your name?")
    val name  = StdIn.readLine()
    val state = State(name)
    gameLoop(state)
  }
}