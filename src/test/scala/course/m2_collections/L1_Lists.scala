package course.m2_collections

import zio.test._
import zio.test.TestAspect._
import course.Lesson

/** For loops are common in other programming languages. In functional Scala,
  * however, most for loops are replaced by operations on Scala collections,
  * which provide high-level and declarative solutions to many problems
  * involving data processing and transformation.
  *
  * Mastery of the Scala collections library can provide an enormous boost to
  * productivity, as well as cement understanding of the tools of the
  * experienced functional programmer (mapping, filtering, folding, etc.).
  *
  * In this module, you will become more familiar with the power of the Scala
  * collections library as you solve a variety of common tasks using the
  * collections operations.
  */

object ListOperations extends Lesson {

  /** ✏ EXERCISE
    *
    * Using `List#foreach`, add up the contents of the provided list into the
    * variable `sum`.
    */
  val foreachTest = test("foreach") {
    var sum = 0

    val list = List(0, 3, 0, 2, 1)

    list.foreach { x =>
      sum += x
    }

    assertTrue(sum == 6)
  }

  /** ✏ EXERCISE
    *
    * Using `List#map`, transform the provided list into a new list, where each
    * element of the list has been multiplied by 2.
    */
  val mapTest = test("map") {
    val list1 = List(0, 3, 0, 2, 1)

    val list2 = list1.map(i => i * 2) // EDIT HERE

    assertTrue(list2.sum == 12 && list2.length == list1.length)
  }

  /** ✏ EXERCISE
    *
    * Using `List#filter`, filter the provided list so that it only contains
    * even numbers.
    */
  val filterTest = test("filter") {
    val isEven = (i: Int) => i % 2 == 0

    val list1 = List(0, 3, 0, 2, 1)

    val list2 = list1.filter(_ % 2 == 0) // EDIT HERE

    assertTrue(list2 == List(0, 0, 2))
  }

  /** ✏ EXERCISE
    *
    * Use `List#take` to take the first 2 elements of the provided list.
    */
  val takeTest = test("take") {
    val list1 = List(1, 2, 3, 4)

    val list2 = list1.take(2)

    assertTrue(list2 == List(1, 2))
  }

  /** ✏ EXERCISE
    *
    * Use `List#takeWhile` to take elements for as long as they are strictly
    * less than 3.
    */
  val takeWhileTest = test("takeWhile") {
    val list1 = List(1, 2, 0, 3, 1, 2)

    val list2 = list1.takeWhile(_ < 3)

    assertTrue(list2 == List(1, 2, 0))
  }

  /** ✏ EXERCISE
    *
    * Use `List#drop` to drop the first 2 elements of the provided list.
    */
  val dropTest = test("drop") {
    val list1 = List(1, 2, 3, 4)

    val list2 = list1 drop 2

    assertTrue(list2 == List(3, 4))
  }

  /** ✏ EXERCISE
    *
    * Use `List#dropWhile` to drop elements for as long as they are strictly
    * less than 3.
    */
  val dropWhileTest = test("dropWhile") {
    val list1 = List(1, 2, 0, 3, 1, 2)

    val list2 = list1 dropWhile (_ < 3)

    assertTrue(list2 == List(3, 1, 2))
  }

  /** ✏ EXERCISE
    *
    * Using `List#collect` and a partial function, collect all even numbers from
    * the provided List, wrapping them into an `Even` wrapper type.
    */
  val collectTest = test("collect") {
    final case class Even(number: Int)

    val isEven = (i: Int) => i % 2 == 0

    val list1 = List(0, 3, 0, 2, 1)

    val partialFunction: PartialFunction[Int, Even] = {
      case x if (isEven(x)) => Even(x)
    }

    def list2: List[Even] = list1.collect(partialFunction)

    assertTrue(list2 == List(Even(0), Even(0), Even(2)))
  }

  /** ✏ EXERCISE
    *
    * Using `partition`, partition the provided list of integers into those that
    * are even, and those that are odd.
    */
  val partitionTest = test("partition") {
    val isEven = (i: Int) => i % 2 == 0

    val list = List(0, 3, 0, 2, 1)

    val (even, odd) = list.partition(isEven)

    assertTrue(even == List(0, 0, 2) && odd == List(3, 1))
  }

  /** ✏ EXERCISE
    *
    * Using `reduceOption`, sum up both of the provided lists. In what cases
    * does `reduceOption` return `None`?
    */
  val reduceOptionTest = test("reduceOption") {
    val list1: List[Int] = List()
    val list2: List[Int] = List(1, 2, 3)

    def summedList1: Option[Int] = list1.reduceOption(_ + _)

    def summedList2: Option[Int] = list2.reduceOption(_ + _)

    assertTrue(
      summedList1 == None,
      summedList2 == Some(6)
    )
  }

  /** ✏ EXERCISE
    *
    * Using `List#find`, find the first number that is greater than two in the
    * provided list.
    */
  val findTest = test("find") {
    val list = List(1, 2, 3, 4)

    def firstGreaterThan2: Option[Int] = list.find(_ > 2)

    assertTrue(firstGreaterThan2 == Some(3))
  }

  /** ✏ EXERCISE
    *
    * Using `List#exists`, test to see if there exists an element of the list
    * that is negative.
    */
  val existsTest = test("exists") {
    val list = List(1, 2, 3, 4, -1, 5)

    def existsNegative: Boolean = list.exists(_ < 0)

    assertTrue(existsNegative)
  }

  /** ✏ EXERCISE
    *
    * Using `List#forall`, test to see if all elements of the list are even
    * numbers.
    */
  val forallTest = test("forall") {
    val isEven = (i: Int) => i % 2 == 0

    val list = List(0, 2, 6, 8, 12, 10)

    def forallEven: Boolean = list.forall(isEven)

    assertTrue(forallEven)
  }

  def exercise =
    suite("Operations")(
      foreachTest,
      filterTest,
      takeTest,
      takeWhileTest,
      dropTest,
      dropWhileTest,
      collectTest,
      partitionTest,
      reduceOptionTest,
      findTest,
      existsTest,
      forallTest
    )

}

object Folds extends Lesson {

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the sum of a list.
    */
  val sumTest = test("sum") {
    def sum(list: List[Int]): Int = list.foldLeft(0)(_ + _)

    assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the maximum element of a list.
    */
  val maxTest = test("max") {
    def max(list: List[Int]): Int = list.foldLeft(Int.MinValue)((a, b) => math.max(a, b))

    assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the minimum element of a list.
    */
  val minTest = test("min") {
    def min(list: List[Int]): Int = list.foldLeft(Int.MaxValue)((a, b) => math.min(a, b))

    assertTrue(min(List(1, 7, 3, 2, 0, 4, 5)) == 0)
  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, compute the reverse of a list.
    */
  val reverseTest = test("reverse") {
    def reverse[A](list: List[A]): List[A] = list.foldLeft(List.empty[A])((a, b) => b :: a)

    assertTrue(reverse(List(1, 7, 3)) == List(3, 7, 1))
  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to partition a list into those
    * satisfying a predicate, and those not satisfying the predicate.
    */
  val partitionTest = test("partition") {

    def partition[A](list: List[A])(pred: A => Boolean): (List[A], List[A]) = {
      val result = list.foldLeft((List.empty[A], List.empty[A]))((a, b) => if (pred(b)) (b :: a._1, a._2) else (a._1, b :: a._2))
      (result._1.reverse, result._2.reverse)
    }

    assertTrue(partition(List(1, 7, 3))(_ < 5) == ((List(1, 3), List(7))))

  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to take `n` elements from a
    * list.
    */
  val takeTest = test("take") {
    def take[A](n: Int, list: List[A]): List[A] =
      list.foldLeft((List.empty[A], 0))((a, b: A) => if (a._2 < n) (b :: a._1, a._2 + 1) else a)._1

    assertTrue(take(2, List(1, 7, 3)) == List(1, 7))
  }

  /** ✏ EXERCISE
    *
    * Using `List#foldLeft`, implement a function to take elements from a list
    * for as long as a predicate is satisfied.
    */
  val takeWhileTest = test("takeWhile") {
    //Issue is unable to halt when issue happens, keeps going, thus need state

    def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] = {
      val init: (Boolean, List[A]) = (true, List.empty[A])

      val res = list.foldLeft(init) {
        case ((true, l), next) if pred(next) =>
          (true, next :: l)

        case ((_, l), _) =>
          (false, l)
      }

      res._2.reverse
    }

    assertTrue(takeWhile(List(1, 7, 3))(_ < 5) == List(1))
  }

  def exercise =
    suite("Folds")(
      sumTest,
      maxTest,
      minTest,
      reverseTest,
      partitionTest,
      takeTest,
      takeWhileTest
    )
}

object Performance extends Lesson {

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val headTailTest = test("head/tail") {
    def sum(values: Seq[Int]): Int =
      values.headOption match {
        case None => 0
        case Some(value) => value + sum(values.drop(1))
      }

    assertTrue(sum(0 to 10000) > 0)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val randomAccessTest = test("random access") {
    def sumProduct(left: Seq[Int], right: Seq[Int]): Int = {
      val length = left.length.max(right.length)

      (0 to length).foldLeft(0) { case (sum, index) =>
        sum + left(index) * right(index)
      }
    }

    assertTrue(sumProduct(List.fill(1000)(2), List.fill(1000)(2)) > 0)
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Investigate and fix the performance problem with this code merely by
    * changing the collection type used.
    */
  val containmentTest = test("containment") {
    def equivalent(left: Seq[Int], right: Seq[Int]): Boolean =
      left.forall(i => right.contains(i)) &&
        right.forall(i => left.contains(i))

    assertTrue(equivalent(List.fill(1000)(2), List.fill(1000)(2)))
  }

  def exercise =
    suite("Performance")(
      headTailTest,
      randomAccessTest,
      containmentTest
    )
}

/** Scala collections library is a highlight of the Scala standard library,
  * containing diverse and high-performance immutable data structures equipped
  * with dozens of helpful operators.
  *
  * In this graduation project, you will gain experience using the Scala
  * collections to implement a graph data structure.
  */
object CollectionsGraduation {

  /** ✏ EXERCISE
    *
    * Using other Scala collections, choose a representation for a graph whose
    * nodes are identified by type `V`, and whose edges are identified by type
    * `E`.
    */
  final case class Graph[E, V]() {

    /** ✏ EXERCISE
      *
      * Implement a function to retrieve the edges connected to the specified
      * node.
      */
    def edgesOf(v: V): Set[E] = ???

    /** ✏ EXERCISE
      *
      * Implement a function to connect the two nodes with the specified edge.
      */
    def connect(v1: V, e: E, v2: V): Graph[E, V] = ???

    /** ✏ EXERCISE
      *
      * Implement a function to cdisonnect the two nodes from the specified
      * edge.
      */
    def disconnect(v1: V, e: E, v2: V): Graph[E, V] = ???

    /** ✏ EXERCISE
      *
      * Implement a function to return the set of all nodes in the graph.
      */
    def nodes: Set[V] = ???

    /** ✏ EXERCISE
      *
      * Implement a function to delete the specified node.
      */
    def delete(v: V): Graph[E, V] = ???

    /** ✏ EXERCISE
      *
      * Implement a function to fold over the nodes, passing at each node both a
      * current state value, and the set of edges connected to the node.
      */
    def fold[Z](initial: Z)(f: (Z, V, Set[E]) => Z): Z = ???
  }

  object Graph {
    def empty[E, V] = Graph()
  }

}
