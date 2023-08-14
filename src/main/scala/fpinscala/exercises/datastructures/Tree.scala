package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + math.max(l.depth, r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int =
    fold(_ => 1, _ + _ + 1)
  
  def depthViaFold: Int =
    fold(_ => 0, math.max(_, _) + 1)
  
  def mapViaFold[B](f: A => B): Tree[B] =
    fold[Tree[B]](a => Leaf(f(a)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(a) => a
    case Branch(l, r) =>
      val lfp = l.firstPositive
      if lfp > 0 then lfp
      else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(a) => a
    case Branch(l, r) => math.max(l.maximum, r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold[Int](identity, math.max(_, _))
