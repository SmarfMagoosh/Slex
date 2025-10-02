package com.slex.parsers

/**
 * data type and parser for parsing a Slex file into an AST
 */
enum SlexAtom:
  case LANG   (header: SlexAtom, rules: SlexAtom)

  case HEADER (name: SlexAtom, kewords: List[SlexAtom], punctuation: List[SlexAtom])

  case RULES  (rules: List[SlexAtom])

  case RULE   (regex: RegexAtom, output: SlexAtom)

  case NAME   (text: String)

  case TOKEN  (text: String)
end SlexAtom
