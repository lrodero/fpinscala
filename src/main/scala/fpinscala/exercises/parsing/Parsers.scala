package fpinscala.exercises.parsing

import scala.annotation.targetName
import scala.util.matching.Regex
import scala.io.AnsiColor.*

case class ParsingResult[+A](result: A, matching: String, remaining: String)

trait Parser[+A]:
  def apply(input: String): Either[ParseError, ParsingResult[A]]
  def run(i: String): Either[ParseError, A] = apply(i).map(_.result)

object Parser:
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String] = str =>
    print(s"Checking ${CYAN}$s${RESET} against input ${CYAN}$str${RESET}")
    val res = Either.cond(str.startsWith(s), ParsingResult(str.substring(0, s.length), str.substring(0, s.length), str.substring(s.length)), ParseError((Location(str), str) :: Nil, Nil))
    println(res.fold(_ => s" ${RED}FAILED!!!${RESET}", _ => s" ${GREEN}ok${RESET}"))
    res
  def stringIgnoreUppercase(s: String): Parser[String] = str =>
    if s.length > str.length then Left(ParseError((Location(str), str) :: Nil, Nil))
    else
      Either.cond(str.substring(0, s.length).equalsIgnoreCase(s), ParsingResult(str.substring(0, s.length), str.substring(0, s.length), str.substring(s.length)), ParseError((Location(str), str) :: Nil, Nil))
  def regex(r: Regex): Parser[String] = str =>
    println(s"Checking regex('$r') against input '$str'")
    val res = r.findPrefixOf(str) match
      case None => Left(ParseError((Location(str), str) :: Nil, Nil))
      case Some(matching) => Right(ParsingResult(matching, matching, str.substring(matching.length)))
    println(res.fold(_ => s" ${RED}FAILED!!!${RESET}", _ => s" ${GREEN}ok${RESET}"))
    res
  def unit[A](a: A): Parser[A] =
    str => Right(ParsingResult(a, "", str))
  def succeed[A](a: A): Parser[A] = unit(a)
  val nonNegativeInt: Parser[Int] = regex("[+]?[0-9]+".r).map(_.toInt)
  val intNumber: Parser[Int] = regex("[+-]?[0-9]+".r).map(_.toInt)
  val doubleNumber: Parser[Double] = regex("[+-]?[0-9]+(\\.[0-9]+)?([Ee][+-]?[0-9]+)?".r).map(_.toDouble)
  val nConsecutiveAs: Parser[Int] = nonNegativeInt.flatMap { i =>
    char('a').listOfN(i).map(_.size)
  }
  val anyCharSaveDoubleQuotes: Parser[String] = regex("[^\"]*".r)
  val whitespaces: Parser[String] = regex("\\s*".r)

  extension [A](p: Parser[A])
    infix def or(p2: => Parser[A]): Parser[A] = str =>
      p(str).orElse(p2(str))
    @targetName("or")
    def |(p2: Parser[A]): Parser[A] = p.or(p2)

    def map[B](f: A => B): Parser[B] =
      flatMap(a => unit(f(a)))

    def flatMap[B](f: A => Parser[B]): Parser[B] = str =>
      p(str).flatMap { pr =>
        f(pr.result)(pr.remaining) match
          case Right(pr2) => Right(ParsingResult(pr2.result, pr.matching + pr2.matching, pr2.remaining))
          case l @ Left(_) => l
      }

    infix def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap { a =>
        p2.map { b =>
          (a, b)
        }
      }
    /** Alias for 'product' */
    @targetName("productt")
    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      product(p2).map((a, b) => f(a,b))

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then unit(Nil)
      else p.map2(p.listOfN(n-1))(_ :: _)

    def many: Parser[List[A]] =
      many1 | unit(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def numA: Parser[Int] = many.map(_.size)

    /** Trim the input string before applying the parsing function */
    def trimmed: Parser[A] = str => p(str.trim)

    /** Parses a list of elements of A separated by the given char */
    def listSeparatedBy(c: Char): Parser[List[A]] =
      (p ** char(c))
        .map(_._1) // drop the separator
        .many
        .map2(p)((init, last) => init :+ last)

/** Return the part of the input string examined. */
    def slice: Parser[String] = str =>
      p(str).map:
        pr =>
          pr.copy(result = pr.matching)
  end extension
end Parser


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
