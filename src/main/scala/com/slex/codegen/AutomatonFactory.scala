package com.slex.codegen

import com.slex.automatons.{DFA, MutableDFA, MutableNFA}
import com.slex.parsers.RegexAtom

import scala.collection.mutable

object AutomatonFactory {
  def keywordDFA(kw: String): DFA = {
    val tfun = Array.fill(kw.length + 2)(
      mutable.Map[Int, Int]()
    )
    kw.zipWithIndex.foreach((input, state) => {
      tfun(state + 1)(input) = state + 2
    })
    DFA(1, Set(kw.length + 1), tfun.map(_.toMap))
  }

  def constructDFA(regex: RegexAtom): DFA = {
    val NFA = new MutableNFA().apply(regex)
    val DFA = new MutableDFA().apply(NFA)
    DFA.reverse.reverse
  }
}