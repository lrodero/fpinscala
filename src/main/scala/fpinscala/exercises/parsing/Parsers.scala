package fpinscala.exercises.parsing

import scala.annotation.targetName

case class ParsingResult[+A](read: A, remaining: String)

trait Parser[+A]:
  def apply(input: String): Either[ParseError, ParsingResult[A]]

extension [A](p: Parser[A])
  infix def or(p2: Parser[A]): Parser[A] = str =>
    p(str).orElse(p2(str))
  @targetName("orr")
  def |(p2: Parser[A]): Parser[A] = p.or(p2)

  def map[B](f: A => B): Parser[B] = str =>
    p(str).map{pr => pr.copy(read = f(pr.read))}

  def flatMap[B](f: A => Parser[B]): Parser[B] = str =>
    p(str).flatMap { pr =>
      f(pr.read)(pr.remaining)
    }

  def product[B](p2: Parser[B]): Parser[(A, B)] =
    p.flatMap { a =>
      p2.map { b =>
       (a, b)
      }
    }

  def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p2).map((a, b) => f(a,b))

  def listOfN(n: Int): Parser[List[A]] =
    if n <= 0 then str => Right(ParsingResult(Nil, str))
    else p.map2(p.listOfN(n-1))(_ :: _)
  //def listOfN(n: Int): Parser[List[A]] = str =>
  //  if n <= 0 then Either.cond(str.isEmpty, Nil, ParseError((Location(str), str) :: Nil, Nil))
  //  else p.run(str).flatMap: a =>
  //    println(s"Found in $str, going for iteration ${n-1}")
  //    listOfN(n-1).run(str.substring(a.)).map(as => a :: as)

object Parser:
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String] = str =>
    println(s"Cheking $s against input $str")
    Either.cond(str.startsWith(s), ParsingResult(s, str.substring(s.length)), ParseError((Location(str), str) :: Nil, Nil))

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A])

  object Laws

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
