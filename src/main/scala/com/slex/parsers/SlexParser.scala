package com.slex.parsers

import java.io.File
import java.util.Scanner
import scala.util.parsing.combinator.RegexParsers

object SlexParser extends RegexParsers {
  def apply(s: String): ParseResult[SlexAtom] = parseAll(lang, s)

  def apply(f: File): ParseResult[SlexAtom] = {
    val scantron = new Scanner(f)
    val sb = new StringBuilder()
    while scantron.hasNextLine do sb.append(scantron.nextLine())
    apply(sb.toString())
  }

  // PARSER RULES
  private def lang = header ~ rules ^^ { case h ~ r => SlexAtom.LANG(h, r) }

  private def header = repN(2, "%") ~> fstRow ~ sndRow ~ thdRow <~ repN(2, "%") ^^ {
    case fst ~ snd ~ thd => SlexAtom.HEADER(fst, snd, thd)
  }

  private def fstRow = "%" ~> "language" ~> name ^^ identity

  private def sndRow = "%" ~> "keywords" ~> rep(token) ^^ identity

  private def thdRow = "%" ~> "punctuation" ~> rep(token) ^^ identity

  private def name = """[A-Z][a-z]*""".r ^^ SlexAtom.NAME.apply

  private def token = """[A-Z]+""".r ^^ SlexAtom.TOKEN.apply

  private def rules = rep1(rule) ^^ SlexAtom.RULES.apply

  private def rule = """([^;]+)|(\\;)""".r <~ ";" ^^ {
    line =>
      val idx = line.lastIndexOf("=>")
      idx match
        case -1 => throw new Exception("Missing '=>' in rule.")
        case x =>
          val pattern = line.substring(0, idx).trim
          val token = line.substring(idx + 2).trim
          RegexParser(pattern) match
            case RegexParser.Success(x, _) => SlexAtom.RULE(x, SlexAtom.TOKEN(token))
            case RegexParser.Failure(msg, remaining) => throw new Exception(s"$msg $remaining")
            case RegexParser.Error(msg, remaining) => throw new Exception(s"$msg $remaining")
  }
}
