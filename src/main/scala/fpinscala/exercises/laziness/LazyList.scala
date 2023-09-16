package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = (n, this) match
    case (i, _) if i <= 0 => Empty
    case (_, Empty) => Empty
    case (i, Cons(h, t)) =>
      lazy val lh = h()
      lazy val lt = t()
      LazyList.cons(lh, lt.take(n - 1))

  def drop(n: Int): LazyList[A] = (n, this) match
    case (i, _) if i <= 0 => this
    case (_, Empty) => Empty
    case (i, Cons(_, t)) =>
      lazy val lt = t()
      lt.drop(i-1)

  def takeWhileWithoutFoldRigh(p: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) if p(h()) =>
      lazy val lh = h()
      lazy val lt = t()
      LazyList.cons(lh, lt.takeWhileWithoutFoldRigh(p))
    case _ => Empty

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A]){ (a, acc) =>
      if p(a) then LazyList.cons(a, acc) else acc
    }

  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false

  def headOptionWithoutFoldRight: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  def headOption: Option[A] =
    foldRight(Option.empty[A]){ (a, acc) =>
      Some(a)
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapWithoutFoldRight[B](f: A => B): LazyList[B] = this match
    case Empty => Empty
    case Cons(h, t) =>
      lazy val lh = f(h())
      lazy val lt = t()
      LazyList.cons(lh, lt.mapWithoutFoldRight(f))

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B]){ (a, acc) =>
      LazyList.cons(f(a), acc)
    }

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A]){ (a, acc) =>
      if f(a) then LazyList.cons(a, acc) else acc
    }

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that){ (a, acc) =>
      LazyList.cons(a, acc)
    }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B]){ (a, acc) =>
      f(a).append(acc)
    }

  def startsWith[B](s: LazyList[B]): Boolean = (this, s) match
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case _ => false

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(i: Int): LazyList[A] =
    unfold((this, i)){
      case (_ ,i) if i <= 0 => None
      case (Empty, _) => None
      case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    zipWith(that){case (a, b) => (a, b)}

  def zipWith[B,C](that: LazyList[B])(f: (A,B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(
        (
          f(h1(), h2()),
          (t1(), t2())
        )
      )
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some(
        (
          (None, Some(h2())),
          (Empty, t2())
        )
      )
      case (Cons(h1, t1), Empty) => Some(
        (
          (Some(h1()), None),
          (t1(), Empty)
        )
      )
      case (Cons(h1, t1), Cons(h2, t2)) => Some(
        (
          (Some(h1()), Some(h2())),
          (t1(), t2())
        )
      )
    }

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def from2(n1: Int, n2: Int): LazyList[Int] = cons(n1 + n2, from2(n2, n1 + n2))
    cons(0, cons(1, from2(0, 1)))
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None => LazyList.empty[A]
    case Some((a, newState)) => LazyList.cons(a, unfold(newState)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    LazyList.cons(0, LazyList.cons(1,
      unfold((0, 1)) { (n1, n2) =>
        val n3 = n1 + n2
        Some((n3, (n2, n3)))
      }
    ))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)( a => Some((a, a+1)) )

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a,())))

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)
