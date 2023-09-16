package fpinscala.exercises.testing

import fpinscala.exercises.laziness.*
import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import fpinscala.exercises.testing.Result.Passed

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successCount: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

import Result.*

//opaque type Prop = TestCases => Result

opaque type SuccessCount <: Int = Int
object SuccessCount:
  private[testing] def apply(sc: Int): SuccessCount = sc

opaque type FailedCase = String
object FailedCase:
  def apply(s: String): FailedCase = s

opaque type Prop = (TestCases, RNG) => Result

extension (p: Prop)
  @targetName("and")
  def &&(that: Prop): Prop =
    (n, rng) => p(n, rng) match
      case Passed => that(n, rng)
      case f: Falsified => f
  @targetName("or")
  def ||(that: Prop): Prop =
    (n, rng) => p(n, rng) match
      case Passed => Passed
      case f: Falsified => that(n, rng)
  def tag(label: String): Prop =
    (n, rng) => p(n, rng) match
      case Passed => Passed
      case Falsified(msg, count) => Falsified(FailedCase(s"$label: $msg"), count)

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    (n, rng) =>
      randomLazyList(gen)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Result.Passed
              else Falsified(a.toString, i)
            catch
              case e: Exception =>
                Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Passed)
  private def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))
  private def buildMsg[A](s: A, e: Exception): String =
    s"""|test case: $s
        |generated an exception: ${e.getMessage}
        |stack trace:
        | ${e.getStackTrace.mkString("\n")}""".stripMargin

opaque type Gen[+A] = Transition[RNG, A]
opaque type SGen[+A] = Int => Gen[A]
import Transition.*
object Gen:
  def unit[A](a: => A): Gen[A] = Transition.unit(a)

  val boolean: Gen[Boolean] = Transition[RNG, Boolean] { (rng: RNG) =>
    val (i, nextRNG) = rng.nextInt
    (i % 2 == 0, nextRNG)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap{ b =>
    if b then g1 else g2
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Transition(RNG.double).flatMap:
    double =>
      val threshold = g1._2 / (g1._2 + g2._2)
      if double < threshold then g1._1 else g2._1

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = Transition[RNG, B] { rng =>
      val (a, newRng) = self.run(rng)
      f(a).run(newRng)
    }
    def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Transition[RNG, Int] { rng =>
      val upperBound = stopExclusive - start
      val (value, newRng) = RNG.nonNegativeLessThan(upperBound).apply(rng)
      (value + start, newRng)
    }
    def listOfN(n: Int): Gen[List[A]] =
      val transitions: List[Transition[RNG, A]] = List.fill(n)(Transition[RNG, A] { rng =>
        self.run(rng)
      })
      Transition.sequence(transitions)

    def unsized: SGen[A] = _ => self

