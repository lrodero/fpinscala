package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

import Parser.*

object JSON:

  val jBool: Parser[JBool] =
    (stringIgnoreUppercase("true") | stringIgnoreUppercase("false")).trimmed.map(b => JBool(b.toBoolean))

  val jDouble: Parser[JNumber] = doubleNumber.trimmed.map(JNumber(_))

  val jString: Parser[JString] =
    (for
      _ <- char('"')
      s <- anyCharSaveDoubleQuotes
      _ <- char('"')
    yield s).trimmed.map(JString(_))

  def arrayElements: Parser[List[JSON]] =
    jValue.trimmed.listSeparatedBy(',')

  val zeroElements: Parser[List[JSON]] = whitespaces.map(_ => Nil)

  val jArray: Parser[JArray] = {
    (for
      _ <- char('[')
      seq <- (arrayElements | zeroElements).trimmed.map(_.toIndexedSeq)
      _ <- char(']').trimmed
    yield seq).trimmed.map(JArray(_))
  }

  val jNull: Parser[JSON] = string("null").trimmed.map(_ => JNull)

  val keyValue: Parser[(String, JSON)] =
    (for
      k <- jString
      _ <- char(':').trimmed
      v <- jValue
    yield k.get -> v).trimmed

  val keyValues: Parser[Map[String, JSON]] =
    keyValue.trimmed.listSeparatedBy(',').map(_.toMap)

  val jObject: Parser[JObject] =
    (for
       _ <- char('{')
       kvs <- keyValues
       _ <- char('}').trimmed
     yield kvs).trimmed.map(JObject(_))

  val jValue = jObject | jArray | jNull | jString | jBool | jDouble

  val jsonParser: Parser[JSON] = jObject | jArray