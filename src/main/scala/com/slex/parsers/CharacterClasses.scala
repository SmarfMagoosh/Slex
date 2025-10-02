package com.slex.parsers

object CharacterClasses {
  val all: Set[Char] = List(' ' to '~', '\t' to '\t', '\n' to '\n').flatten.toSet
  val wildcard: Set[Char] = all &~ Set('\n')
  val word: Set[Char] = List('a' to 'z', 'A' to 'Z', '0' to '9', '_' to '_').flatten.toSet
  val nonword: Set[Char] = all &~ word
  val digit: Set[Char] = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val nondigit: Set[Char] = all &~ digit
  val whitespace: Set[Char] = Set(' ', '\n', '\t')
  val nonwhitespace: Set[Char] = all &~ whitespace
}
