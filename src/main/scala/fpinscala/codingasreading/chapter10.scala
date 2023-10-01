package fpinscala.codingasreading.chapter10

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  val empty: A

val stringMonoid: Monoid[String] = new Monoid[String]:
  override def combine(a1: String, a2: String): String = a1 + a2
  override val empty: String = ""

def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]:
  override def combine(l1: List[A], l2: List[A]): List[A] = l1 ::: l2
  override val empty: List[A] = Nil

