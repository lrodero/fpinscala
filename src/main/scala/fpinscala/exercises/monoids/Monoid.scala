package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*
import jdk.nashorn.internal.runtime.ECMAErrors

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(i1: Int, i2: Int) = i1 + i2
    val empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(i1: Int, i2: Int) = i1 * i2
    val empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(b1: Boolean, b2: Boolean) = b1 | b2
    val empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(b1: Boolean, b2: Boolean) = b1 & b2
    val empty = true

  def optionMonoid[A](using ma: Monoid[A]): Monoid[Option[A]] = new:
    def combine(oa1: Option[A], oa2: Option[A]) =
      for
        a1 <- oa1
        a2 <- oa2
      yield ma.combine(a1, a2)
    val empty = Option(ma.empty)

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f1: A => A, f2: A => A): A => A = f1 andThen f2
    val empty = identity

  import fpinscala.exercises.testing.*

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen): (a: A) =>
      m.combine(a, m.empty) == m.combine(m.empty, a) == a
    &&
      Prop.forAll(gen ** gen ** gen): (a1a2: (A, A), a3: A) =>
        val (a1, a2) = a1a2
        m.combine(m.combine(a1, a2), a3) == m.combine(a1, m.combine(a2, a3))

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldRight(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.empty)( (a, b) => m.combine(f(a), b))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as match
      case _ if as.isEmpty => m.empty
      case _ if as.size == 1 => f(as.head)
      case _ =>
        val (half1, half2) = as.splitAt(as.length/2)
        m.combine(foldMapV(half1, m)(f), foldMapV(half2, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def combine(pa1: Par[A], pa2: Par[A]): Par[A] = pa1.map2(pa2)((a1, a2) => m.combine(a1, a2))
    def empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    val parallel: Monoid[Par[B]] = par(m)
    foldMap[A, Par[B]](v.toList, parallel)(a => Par.unit(f(a)))

  // def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  private def trickyMonoid(h1: IndexedSeq[Int], h2: IndexedSeq[Int]): Monoid[Boolean] = new:
    def combine(b1: Boolean, b2: Boolean): Boolean = b1 && b2 && (h1.last <= h2.head)
    def empty = true
  def ordered(ints: IndexedSeq[Int]): Boolean =
    if ints.size <= 1 then true
    else
      val (h1, h2) = ints.splitAt(ints.length / 2)
      foldMap(h1 :: h2 :: Nil, trickyMonoid(h1, h2))(ordered)

  def ordered2(ints: IndexedSeq[Int]): Boolean =
    if ints.size <= 1 then true
    else
      val (h1, h2) = ints.splitAt(ints.length/2)
      ordered2(h1) && ordered2(h2) && h1.last <= h2.head

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  import WC.*
  lazy val wcMonoid: Monoid[WC] = new:
    val empty: WC = Stub("")
    def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l, w, r), Part(l2, w2, r2)) =>
        val middleWord = if (r + l2).isEmpty then 0 else 1
        Part(l, w + w2 + middleWord, r2)

  def count(s: String): Int =
    def wc(c: Char): WC =
      if c.isWhitespace then
        WC.Part("", 0, "")
      else
        WC.Stub(c.toString)
    def unstub(s: String) = if s.isEmpty then 0 else 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(s) => unstub(s)
      case WC.Part(l, w, r) => unstub(l) + w + unstub(r)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)): (A, B) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
    val empty: (A, B) = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = _ => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keys ++ b.keys).foldLeft(empty) : (acc, k) =>
        val value = mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty))
        acc + (k -> value)
    val empty = Map.empty[K, V]

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    given im: Monoid[Int] = intAddition
    as.foldLeft(mapMergeMonoid.empty){ (acc, a) =>
      mapMergeMonoid.combine(acc, Map(a -> 1))
    }

end Monoid
