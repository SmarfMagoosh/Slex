package com.slex.parsers

/**
 * data type and parser for parsing a regex into an AST
 */
enum RegexAtom:
  case CHAR(c: Char)

  case CONCAT(concats: RegexAtom*)

  case STAR(r: RegexAtom)

  case PLUS(r: RegexAtom)

  case ALTER(rs: RegexAtom*)

  case QMARK(r: RegexAtom)

  case CLASS(cs: Set[Char])

  case GROUP(r: RegexAtom)

  case REPEATEXACT(r: RegexAtom, n: Int)

  case REPEATATLEAST(r: RegexAtom, n: Int)

  case REPEATBETWEEN(r: RegexAtom, n: Int, m: Int)
end RegexAtom
