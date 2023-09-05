package fpinscala.codingasreading.chapter07

import java.util.concurrent.*

def sum(ints: List[Int]): Int =
  ints.foldLeft(0)(_ + _)

def sum2(ints: IndexedSeq[Int]): Int =
  if ints.size <= 1 then
    ints.headOption.getOrElse(0)
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    sum2(l) + sum2(r)

private case class UnitFuture[A](a: A) extends Future[A]:
  override def isDone = true
  override def get(timeout: Long, units: TimeUnit) = a
  override def isCancelled = false
  override def get(): A = a

  override def cancel(eventIfRunning: Boolean) = false

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]{
    def call = a(es).get
  })

extension [A](pa: Par[A])
  def run(s: ExecutorService): Future[A] = pa(s)
  def map2[B, C](pb: => Par[B])(f: (A, B) => C): Par[C] =
    ex =>
      val fa = pa(ex)
      val fb = pb(ex)
      UnitFuture(f(fa.get, fb.get))

