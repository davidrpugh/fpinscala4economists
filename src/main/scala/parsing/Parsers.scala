package parsing

import testing.{Gen, Prop}

import java.util.regex._
import scala.util.matching.Regex


trait Parsers[ParseError, Parser[+_]] {
  self =>

  def attempt[A](p: Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = {
    string(c.toString).map (s => s.charAt(0))
  }

  def doubleString: Parser[String] = {
      token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)
  }

  def double: Parser[Double] = {
      doubleString map (_.toDouble)
  }

  def eof: Parser[String] = {
    regex("\\z".r)
  }

  def escapedQuoted: Parser[String] = {
    token(quoted)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) {
      succeed(List.empty)
    } else {
      map2(p, listOfN(n - 1, p))((a, as) => a :: as)
    }
  }

  // exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = {
    many1(p) or succeed(List.empty)
  }

  // exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a, as) => a :: as)
  }

  // exercise 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(a => succeed(f(a)))
  }

  // exercise 9.7
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    flatMap(p1)(a => map(p2)(b => f(a, b)))
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // exercise 9.1 & exercise 9.7
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    flatMap(p1)(a => map(p2)(b => (a, b)))
  }

  def quoted: Parser[String] = {
    skipL(string("\""), thru("\"")).map(_.dropRight(1))
  }

  def root[A](p: Parser[A]): Parser[A] = {
    skipR(p, eof)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def sep1[A](p1: Parser[A], p2: Parser[Any]): Parser[List[A]] = {
    map2(p1, many(skipL(p2, p1)))( _ :: _)
  }

  def sep[A](p1: Parser[A], p2: Parser[Any]): Parser[List[A]] = {
    sep1(p1, p2).or(succeed(List()))
  }

  def skipL[A](p1: Parser[Any], p2: => Parser[A]): Parser[A] = {
    map2(slice(p1), p2)((_, a) => a)
  }

  def skipR[A](p1: Parser[A], p2: => Parser[Any]): Parser[A] = {
    map2(p1, slice(p2))((a, _) => a)
  }

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) = {
    skipR(skipL(start, p), stop)
  }

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def token[A](p: Parser[A]): Parser[A] = {
    skipR(attempt(p), whitespace)
  }

  def whitespace: Parser[String] = {
    "\\s*".r
  }

  implicit def ops[A](p: Parser[A]): ParserOps[A] = {
    ParserOps[A](p)
  }

  implicit def regex(r: Regex): Parser[String]

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = {
    ParserOps(f(a))
  }


  case class ParserOps[A](p: Parser[A]) {

    def | [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def ** [B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def or(p2: => Parser[A]): Parser[A] = self.or(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def sep(p2: Parser[Any]): Parser[List[A]] = self.sep(p, p2)

    def slice: Parser[String] = self.slice(p)

  }


  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(g: Gen[String]): Prop = {
      Prop.forAll(g)(s => run(p1)(s) == run(p2)(s))
    }

    def map[A](p: Parser[A])(g: Gen[String]): Prop = {
      equal(p, p.map(identity))(g)
    }

  }

}
