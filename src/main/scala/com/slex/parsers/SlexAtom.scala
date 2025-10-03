package com.slex.parsers

/**
 * data type and parser for parsing a Slex file into an AST
 */
enum SlexAtom:
  case LANG   (header: SlexAtom.HEADER, rules: SlexAtom.RULES)

  case HEADER (name: SlexAtom.NAME, keywords: List[SlexAtom.TOKEN], punctuation: List[SlexAtom.TOKEN])

  case RULES  (rules: List[SlexAtom.RULE])

  case RULE   (regex: RegexAtom, output: SlexAtom.TOKEN)

  case NAME   (text: String)

  case TOKEN  (text: String)
end SlexAtom
