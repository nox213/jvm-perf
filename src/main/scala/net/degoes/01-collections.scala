/**
 * COLLECTIONS
 *
 * Thanks to powerful abstractions on the JVM, including java.util Collections, or standard library
 * collections in Scala, Kotlin, and other JVM-based languages, it is easy to write code that
 * processes data in bulk.
 *
 * With this ease comes a danger: it is easy to write code that is not performant. This performance
 * cost comes about because of several factors:
 *
 *   1. Wrong collection type. Different collection types have different overhead on different kinds
 *      of operations. For example, doubly-linked linked lists are good at prepending and appending
 *      single elements, but are terrible at random access.
 *
 * 2. Boxing of primitives. On the JVM, primitives are not objects, and so they must be boxed into
 * objects in order to be stored in collections. This boxing and unboxing can be expensive.
 *
 * 3. Cache locality. Modern CPUs are very fast, but they are also very complex. One of the ways
 * that CPUs achieve their speed is by caching data in memory. Most collection types do not store
 * their elements contiguously in memory, even if they are primitives, and so cannot take advantage
 * of the CPU cache, resulting in slower performance.
 *
 * In this section, you will use the JMH benchmarking tool in order to explore collection
 * performance across a range of collections, and then you will discover not only how to use the
 * fastest collection type but how to increase its applicability to a wider range of use cases.
 */
package net.degoes.collections

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import zio.Chunk
import cats.data.{ Chain, NonEmptyList }

/**
 * EXERCISE 1
 *
 * This benchmark is currently configured with List, which is a Scala linked-list data type. Add two
 * other collection types to this benchmark (in Scala, choose Vector and Array; if completing these
 * exercises in another programming language, be sure to at least choose Array).
 *
 * EXERCISE 2
 *
 * Identify which collection is the fastest for prepending a single element, and explain why.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class ElementPrependBenchmark {
  val PrependsPerIteration = 100

  @Param(Array("1000", "10000", "100000"))
  var size: Int = _

  var startList: List[String]                 = _
  var startChunk: Chunk[String]               = _
  var startChain: Chain[String]               = _
  var startNonEmptyList: NonEmptyList[String] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    startList = List.fill(size)("a")
    startChain = Chain.apply(List.fill(size)("a"): _*)
    startNonEmptyList = NonEmptyList.fromListUnsafe(List.fill(size)("a"))
    startChunk = Chunk.fill(size)("a")
  }

  @Benchmark
  def list(blackhole: Blackhole): Unit =
    blackhole.consume("a" :: startList)

  @Benchmark
  def nonEmptyList(blackhole: Blackhole): Unit =
    blackhole.consume("a" :: startNonEmptyList)

  @Benchmark
  def chain(blackhole: Blackhole): Unit =
    blackhole.consume("a" +: startChain)

  def chunk(blackhole: Blackhole): Unit =
    blackhole.consume("a" +: startChunk)
}

/**
 * EXERCISE 2
 *
 * Create a benchmark for concatenation across lists, vectors (or some other standard collection
 * type, if not solving these problems in Scala), and arrays.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class ConcatBenchmark {
  @Param(Array("1000", "10000", "100000"))
  var size: Int = _

  var startList: List[String]                 = _
  var startChunk: Chunk[String]               = _
  var startChain: Chain[String]               = _
  var startNonEmptyList: NonEmptyList[String] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    startList = List.fill(size)("a")
    startChain = Chain.apply(List.fill(size)("a"): _*)
    startNonEmptyList = NonEmptyList.fromListUnsafe(List.fill(size)("a"))
    startChunk = Chunk.fill(size)("a")
  }

  @Benchmark
  def list(blackhole: Blackhole): Unit =
    blackhole.consume(startList ++ startList)

  @Benchmark
  def nonEmptyList(blackhole: Blackhole): Unit =
    blackhole.consume(startNonEmptyList.concat(startList))

  @Benchmark
  def chain(blackhole: Blackhole): Unit =
    blackhole.consume(startChain ++ startChain)

  @Benchmark
  def chunk(blackhole: Blackhole): Unit =
    blackhole.consume(startChunk ++ startChunk)
}

/**
 * EXERCISE 3
 *
 * Create a benchmark for random access across lists, vectors (or some other standard collection
 * type, if not solving these problems in Scala), and arrays.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class RandomAccessBenchmark {
  @Param(Array("1000", "10000", "100000"))
  var size: Int = _

  var startList: List[String]   = _
  var startChunk: Chunk[String] = _
  var startChain: Chain[String] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    startList = List.fill(size)("a")
    startChain = Chain.apply(List.fill(size)("a"): _*)
    startChunk = Chunk.fill(size)("a")
  }

  @Benchmark
  def list(blackhole: Blackhole): Unit =
    blackhole.consume(startList(size / 2))

  @Benchmark
  def chain(blackhole: Blackhole): Unit =
    blackhole.consume(startChain.get(size / 2))

  @Benchmark
  def chunk(blackhole: Blackhole): Unit =
    blackhole.consume(startChunk(size / 2))
}

/**
 * EXERCISE 4
 *
 * Create a benchmark for iteration, which sums all the elements in a collection, across lists,
 * vectors (or some other standard collection type, if not solving these problems in Scala), and
 * arrays.
 *
 * NOTE: Arrays of primitives are specialized on the JVM. Which means they do not incur overhead of
 * "boxing", a topic we will return to later. For now, just make sure to store java.lang.Integer
 * values in the Array in order to ensure the benchmark is fair.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class IterationBenchmark {
  @Param(Array("1000", "10000", "100000"))
  var size: Int = _

  var startList: List[String]   = _
  var startChunk: Chunk[String] = _
  var startChain: Chain[String] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    startList = List.fill(size)("a")
    startChain = Chain.apply(List.fill(size)("a"): _*)
    startChunk = Chunk.fill(size)("a")
  }

  @Benchmark
  def listForeach(blackhole: Blackhole): Unit =
    startList.foreach(blackhole.consume)

  @Benchmark
  def listIterator(blackhole: Blackhole): Unit = {
    val it = startList.iterator
    while (it.hasNext) blackhole.consume(it.next())
  }

  @Benchmark
  def chunk(blackhole: Blackhole): Unit = {
    var i = 0
    while (i < startChunk.length) {
      blackhole.consume(startChunk(i))
      i = i + 1
    }
  }

  @Benchmark
  def chain(blackhole: Blackhole): Unit =
    startChain.foldLeft(())((_, a) => blackhole.consume(a))

}

/**
 * EXERCISE 5
 *
 * Create a benchmark for lookup of an element by a property of the element, across lists, arrays,
 * and maps.
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class LookupBenchmark {
  val Size       = 1000
  val IdToLookup = Size - 1

  case class Person(id: Int, age: Int, name: String)
  val peopleList: List[Person] = List.tabulate(Size)(i => Person(i, i, s"Person $i"))

  @Setup(Level.Trial)
  def setup(): Unit = ()

  @Benchmark
  def list(blackhole: Blackhole): Unit =
    blackhole.consume(peopleList.find(_.id == IdToLookup).get)
}

/**
 * GRADUATION PROJECT
 *
 * Develop a new immutable collection type (`Chain`) that has O(1) for concatenation. Compare its
 * performance to at least two other collection types. Then augment this collection type with
 * iteration, so you can benchmark iteration against the other collection types.
 *
 * Think carefully about whether or not it is possible to have a single collection type that has
 * best-in-class performance across all operations. Why or why not?
 */
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(16)
class GraduationBenchmark {
  @Param(Array("100", "1000", "10000"))
  var size: Int = _

  @Benchmark
  def concatCatsChain(blackhole: Blackhole): Unit = {
    var i = 0
    var c = Chain(1)
    while (i < size) {
      c = c ++ c
      i = i + 1
    }
    blackhole.consume(c)
  }

  @Benchmark
  def concatMyChain(blackhole: Blackhole): Unit = {
    var i = 0
    var c = MyChain(1)
    while (i < size) {
      c = c ++ c
      i = i + 1
    }
    blackhole.consume(c)
  }

  sealed trait MyChain[+A] {
    def ++[A1 >: A](that: MyChain[A1]): MyChain[A1] = MyChain.Concat(this, that)
  }

  object MyChain {
    case class Pure[A](a: A*)                                 extends MyChain[A]
    case class Concat[A](left: MyChain[A], right: MyChain[A]) extends MyChain[A]

    def empty: MyChain[Nothing] = MyChain()

    def apply[A](as: A*): MyChain[A] = MyChain.Pure(as: _*)
  }
}
