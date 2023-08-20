package fpinscala.codingasreading.chapter02

import scala.util.control.NonFatal


enum Option[+A]:
  case Some(get: A)
  case None

def mean(as: Seq[Double]): Option[Double] =
  import Option.*
  if as.isEmpty then None
  else Some(as.sum / as.size)

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

def catchNonFatal[A, B](f:A => B): A => Either[Throwable, B] = a =>
  import Either.*
  try Right(f(a))
  catch case NonFatal(t) => Left(t)
