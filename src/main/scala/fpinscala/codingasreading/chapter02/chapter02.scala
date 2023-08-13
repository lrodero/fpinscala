package fpinscala.codingasreading.chapter02

// Just to test that I can run code using IntelliJ and scala-cli:
// $ scala-cli run src/main/scala/fpinscala/codingasreading/chapter02/chapter02.scala
// (or)
// $ scala-cli console .
// scala> import fpinscala.codingasreading.chapter02.MyProgram
// scala> MyProgram.printAbs
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x: Int) =
    val msg = "The absolute values of %d is %d"
    msg.format(x, abs(x))

  @main def print: Unit=
    println(isSorted(Array(1,2,3), _ > _))
    println(isSorted(Array(3,2,1), _ > _))
    println(isSorted(Array.empty[Int], _ > _))
    println(isSorted(Array(1), _ > _))
    println(isSorted(Array(1,2), _ > _))
    println(isSorted(Array(2,1), _ > _))
    println(isSorted(Array(100,-100), _ > _))
    println(isSorted(Array(0,0), _ > _))

def factorial(n: Int): Int =
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if n <= 0 then acc
    else go(n-1, n * acc)
  go(n, 1)

def findFirst[A](array: Array[A], check: A => Boolean): Int =
  @annotation.tailrec
  def loop(i: Int): Int =
    if i == array.length then -1
    else if check(array(i)) then i
    else loop(i+1)
  loop(0)

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def subIsSorted(i: Int): Boolean =
    if as.size <= i + 1 then true
    else if !gt(as(i), as(i + 1)) then false
    else subIsSorted(i + 1)
  subIsSorted(0)
