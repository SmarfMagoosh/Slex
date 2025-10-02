package com.slex.parsers

//import com.slex.parsers.RegexParser.{opt, parseAll, rep1, rep1sep}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object RegexParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def apply(s: String): ParseResult[RegexAtom] = parseAll(alter, s)

  private val outclass: Regex = """[ -',\-/->@-Z^-z~]""".r
  private val outclassEsc: Regex = """\\[nt()+*?.{\[|\\]""".r
  private val inclass: Regex = """[ -,.-\[^-~]""".r
  private val inclassEsc: Regex = """\\[nt\\\-\[\]]""".r
  private val numRegex: Regex = """[1-9][0-9]*""".r

  // helper methods
  private def esc: String => Char =
    case "\\n" => '\n'
    case "\\t" => '\t'
    case "\\\\" => '\\'
    case "\\-" => '-'
    case "\\[" => '['
    case "\\]" => ']'
    case "\\(" => '('
    case "\\)" => ')'
    case "\\+" => '+'
    case "\\*" => '*'
    case "\\?" => '?'
    case "\\." => '.'
    case "\\{" => '{'
    case "\\|" => '|'
    case x => x.head

  // parser rules
  def alter: Parser[RegexAtom] = rep1sep(concat, "|") ^^ { x => RegexAtom.ALTER(x: _*) }

  private def cclass: Parser[Set[Char]] =
    "." ^^ { _ => CharacterClasses.wildcard }
      | "\\w" ^^ { _ => CharacterClasses.word }
      | "\\W" ^^ { _ => CharacterClasses.nonword }
      | "\\d" ^^ { _ => CharacterClasses.digit }
      | "\\D" ^^ { _ => CharacterClasses.nondigit }
      | "\\s" ^^ { _ => CharacterClasses.whitespace }
      | "\\S" ^^ { _ => CharacterClasses.nonwhitespace }
      | "[" ~> opt("^") ~ rep1(charset) <~ "]" ^^ {
      case None ~ sets => sets.flatten.map(_.toChar).toSet
      case Some(_) ~ sets => CharacterClasses.all diff sets.flatten.map(_.toChar).toSet
    }

  private def charset: Parser[Range] = char ~ ("-" ~> char).? ^^ {
    case c ~ None => c.toInt to c.toInt
    case c ~ Some(o) => c.toInt to o.toInt
  }

  private def char: Parser[Char] =
    inclass ^^ { _.head }
    | inclassEsc ^^ esc

  private def modifier: Parser[RegexAtom => RegexAtom] =
    "*" ^^^ RegexAtom.STAR.apply
      | "+" ^^^ RegexAtom.PLUS.apply
      | "?" ^^^ RegexAtom.QMARK.apply
      | "{" ~> numRegex <~ "}" ^^ { n => r => RegexAtom.REPEATEXACT(r, n.toInt) }
      | "{" ~> numRegex <~ "," <~ "}" ^^ { n => r => RegexAtom.REPEATATLEAST(r, n.toInt) }
      | "{" ~> numRegex ~ "," ~ numRegex <~ "}" ^^ { case f ~ _ ~ s => r => RegexAtom.REPEATBETWEEN(r, f.toInt, s.toInt) }

  private def base: Parser[RegexAtom] =
    cclass ^^ RegexAtom.CLASS.apply
      | outclass ^^ { esc andThen RegexAtom.CHAR.apply }
      | outclassEsc ^^ { esc andThen RegexAtom.CHAR.apply }
      | '(' ~> alter <~ ')' ^^ RegexAtom.GROUP.apply

  private def concat: Parser[RegexAtom] =
    rep1(base ~ opt(modifier)) ^^ { bases =>
      RegexAtom.CONCAT(bases.map({
        case x ~ None => x
        case x ~ Some(mod) => mod(x)
      }): _*)
    }
}
