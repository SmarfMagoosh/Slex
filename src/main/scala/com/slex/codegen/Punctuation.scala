package com.slex.codegen

object Punctuation {
  val puncToAlias : Map[String, List[String]] = Map(
    "~" -> List("TILDE", "SQUIGGLE"),
    "`" -> List("BACKTICK", "BTICK"),
    "!" -> List("EXCLAMATION", "EXCLAM"),
    "@" -> List("ATSYMBOL", "AT", "ATSYMB"),
    "#" -> List("POUND", "HASHTAG", "NUM", "NUMBER", "HASH", "SHARP"),
    "$" -> List("DOLLAR", "DOLLARSIGN", "DSIGN"),
    "%" -> List("PERC", "PERCENT", "MOD"),
    "^" -> List("CARAT", "XOR"),
    "&" -> List("AND", "AMPERSAND", "AMP"),
    "*" -> List("STAR", "ASTERISK", "TIMES"),
    "(" -> List("LPAREN", "LEFTPAREN", "LEFTPARENTHESES", "LPAR"),
    ")" -> List("RPAREN", "RIGHTPAREN", "RIGHTPARENTHESES", "RPAR"),
    "-" -> List("HYPHEN", "DASH", "MINUS"),
    "_" -> List("UNDERSCORE", "USCORE", "UNDER"),
    "+" -> List("PLUS"),
    "=" -> List("EQ", "EQUAL", "EQUALS"),
    "{" -> List("LBRACE", "LEFTBRACE"),
    "}" -> List("RBRACE", "RIGHTBRACE"),
    "[" -> List("LBRACK", "LEFTBRACK", "LBRACKET", "LEFTBRACKET"),
    "]" -> List("RBRACK", "RIGHTBRACK", "RBRACKET", "RIGHTBRACKET"),
    "|" -> List("PIPE", "OR"),
    "\\" -> List("BACKSLASH", "BSLASH"),
    ":" -> List("COLON"),
    ";" -> List("SEMICOLON"),
    "\'" -> List("APOSTROPHE", "SINGLEQUOTE", "SQUOTE"),
    "\"" -> List("QUOTE", "DOUBLEQUOTE"),
    "," -> List("COMMA"),
    "<" -> List("LESSTHAN", "LESSER", "LT"),
    "." -> List("DOT", "PERIOD"),
    ">" -> List("GREATERTHAN", "GREATER", "GT"),
    "/" -> List("FORWARDSLASH", "FSLASH", "DIV", "DIVIDE"),
    "?" -> List("QUESTION", "QUESTIONMARK", "QMARK"),
  )

  val aliasToPunc : Map[String, String] = puncToAlias.flatMap({
    case (k, v) => for alias <- v yield (alias, k)
  })
}
