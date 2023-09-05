package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop:
  self =>

  @targetName("and")
  def &&(that: Prop): Prop = new Prop:
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      for {
        successCount1 <- self.check
        successCount2 <- that.check
      } yield SuccessCount(successCount1 + successCount2)

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

object Prop:
  opaque type SuccessCount <: Int = Int
  object SuccessCount:
    private[testing] def apply(sc: Int): SuccessCount = sc
  opaque type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

opaque type Gen[+A] = Transition[RNG, A]
import Transition.*

object Gen:

  def unit[A](a: => A): Gen[A] = Transition.unit(a)

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


trait SGen[+A]
