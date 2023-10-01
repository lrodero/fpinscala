package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual, listMonoid}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      ???

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      foldLeft(mb.empty): (acc, a) =>
        mb.combine(acc, f(a))

    def combineAll(using ma: Monoid[A]): A =
      foldMap(identity)

    def toList: List[A] =
      foldMap[List[A]](a => List(a))(using listMonoid[A])

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as.foldLeft(mb.empty) : (acc, a) =>
          mb.combine(acc, f(a))

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case Leaf(a) => f(a, acc)
        case Branch(lt, rt) => lt.foldRight(rt.foldRight(acc)(f))(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case Leaf(a) => f(acc, a)
        case Branch(lt, rt) => rt.foldLeft(lt.foldLeft(acc)(f))(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Leaf(a) => f(a)
        case Branch(lt, rt) => mb.combine(lt.foldMap(f), rt.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case None => acc
        case Some(a) => f(a, acc)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case None => acc
        case Some(a) => f(acc, a)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        foldLeft(mb.empty) : (acc, a) =>
          mb.combine(acc, f(a))
