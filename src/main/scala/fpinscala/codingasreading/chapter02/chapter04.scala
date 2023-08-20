package fpinscala.codingasreading.chapter02


enum Option[+A]:
  case Some(get: A)
  case None

def mean(as: Seq[Double]): Option[Double] =
  import Option.*
  if as.isEmpty then None
  else Some(as.sum / as.size)

