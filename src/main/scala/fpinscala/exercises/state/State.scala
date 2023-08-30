package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  @annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match
    case (i, newRng) if i >= 0 => (i, newRng)
    case (i, newRng) if i < 0 && i > Int.MinValue => (-i, newRng)
    case (_, newRng) => nonNegativeInt(newRng) // i is Int.MinValue, we have to regenerate

  @annotation.tailrec
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match
    case (i, newRng) if i < Int.MaxValue => (i.toDouble/Int.MaxValue.toDouble, newRng)
    case (_, newRng) => double(newRng) // i is Int.MaxValue, we have to regenerate

  def _double(rng: RNG): (Double, RNG) =
    def nonMaxValueInt(rng: RNG): RNG = new RNG:
      override def nextInt: (Int, RNG) = nonNegativeInt(rng) match
        case (Int.MaxValue, newRng) => nonMaxValueInt(newRng).nextInt
        case (i, newRng) => (i, newRng)
    val gen: Rand[Double] = map(_.nextInt)(_.toDouble / Int.MaxValue.toDouble)
    gen(nonMaxValueInt(rng))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, rndg1) = rng.nextInt
    val (d, rndg2)  = double(rndg1)
    ((i, d), rndg2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), rndg) = intDouble(rng)
    ((d, i), rndg)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def go(accL: List[Int], pending: Int, rng: RNG): (List[Int], RNG) =
      if pending <= 0 then (accL, rng)
      else
        val (i, rng2) = rng.nextInt
        go(i +: accL, pending - 1, rng2)
    go(Nil, count, rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldRight((List.empty[A], rng)){ (rand, state) =>
        val (i, nextRng) = rand(state._2)
        (i :: state._1, nextRng)
      }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if i + (n-1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      flatMap(rb){ b =>
        unit(f(a, b))
      }
    }

  def rollDice: Rand[Int] = nonNegativeLessThan(6)

opaque type Transition[S, +A] = S => (A, S)

object Transition:
  extension [S, A](underlying: Transition[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): Transition[S, B] =
      s =>
        val (a, newS) = run(s)
        (f(a), newS)

    def map2[B, C](sb: Transition[S, B])(f: (A, B) => C): Transition[S, C] =
      s1 =>
        val (a, s2) = run(s1)
        val (b, s3) = sb.run(s2)
        (f(a,b), s3)

    def flatMap[B](f: A => Transition[S, B]): Transition[S, B] =
      s1 =>
        val (a, s2) = run(s1)
        f(a).run(s2)

  def sequence[S, A](automatas: List[Transition[S, A]]): Transition[S, List[A]] =
    s => automatas.foldRight((List.empty[A], s)){ (automata, acc) =>
      val list = acc._1
      val state = acc._2
      val (a, newState) = automata.run(state)
      (list :+ a, newState)
    }

  def traverse[S, A, B](as: List[A])(f: A => Transition[S, B]): Transition[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def apply[S, A](f: S => (A, S)): Transition[S, A] = f

  def unit[S, A](a: A): Transition[S, A] = s => (a, s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  import Input.*
  import Transition.*
  def sim(input: Input): Transition[Machine, (Int, Int)] = Transition[Machine, (Int, Int)] {
    machine =>
      println(s"Machine: $machine; input: $input")
      input match
        case _ if machine.candies == 0 => ((machine.coins, 0), machine)
        case Coin if machine.locked =>
          ((machine.coins + 1, machine.candies), machine.copy(locked = false, coins = machine.coins + 1))
        case Coin => ((machine.coins, machine.candies), machine)
        case Turn if !machine.locked =>
          ((machine.coins, machine.candies - 1), machine.copy(locked = true, candies = machine.candies - 1))
        case Turn => ((machine.coins, machine.candies), machine)
  }
  def simulateMachine(inputs: List[Input]): Transition[Machine, (Int, Int)] = Transition[Machine, (Int, Int)] {
    machine =>
      inputs match
        case Nil => ((machine.coins, machine.candies), machine)
        case inputs =>
          val (outputs, newMachine) = Transition.sequence(inputs.reverse.map(sim)).run(machine)
          (outputs.last, newMachine)
  }

