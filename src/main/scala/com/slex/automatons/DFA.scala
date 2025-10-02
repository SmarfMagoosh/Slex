package com.slex.automatons

import scala.collection.mutable
import scala.collection.mutable.Set as MSet

/**
 * state 0 is always assumed to be the trap state
 * @param start the starting state for the DFA
 * @param accepting the set of accepting states for the DFA
 * @param tfun the transition function for the DFA
 */
class DFA (start: Int = 1, accepting: Set[Int], tfun: Array[Map[Int, Int]]) {
  private var currState = start

  def isTrapped: Boolean = currState == 0

  def isAccepting: Boolean = accepting contains currState

  def process(input: Char): Unit = {
    currState = tfun(currState)(input)
  }

  def reset(): Unit = {
    currState = start
  }

  override def toString: String = {
    def getRow(i: Int): Map[Int, Int] = tfun(i).filter((s, i) => s != 0)
    def toChar(i: Int): String = i.toChar match {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\u000B' => "\\v" // vertical tab
      case ' '  => "\" \""
      case c if c.isWhitespace => "\"" + c + "\""
      case c => c.toString
    }
    def rowToString(i: Int): String = {
      getRow(i).toList.map ((k, v) => s"\t($i, $k) => $v\n").mkString
    }
    s"""
       |Start States:
       |\t$start\n
       |Accepting States:
       |\t$accepting\n
       |Transition Function:
       |${tfun.indices.map(rowToString).mkString}
       |""".stripMargin
  }

  def serialize(t: String): String = {
    val accString = accepting.mkString(", ")
    val tfunString = tfun
      .map(row => {
        val str = if row.isEmpty then "" else row.map({ case (k, v) => s"$k,$v," }).mkString
        s"${row.size},$str"
      })
      .mkString("\n", "\n", "")
    s"$t,$start,${accepting.size},$accString,${tfun.length},$tfunString\n"  
  }

  def reverse: DFA = {
    val tfun_prime = {
      val x = Array.fill(tfun.length)(
         mutable.Map[Int, MSet[Int]]()
       )
      for i <- tfun.indices; j <- tfun(i).keys do {
        val y = x(tfun(i)(j))
        if y contains j then {
          y(j).add(i)
        } else {
          y.put(j, mutable.Set(i))
        }
      }
      x.map(row => row.map({
        case (k, v) => (k, v.toSet)
      }).toMap)
    }
    val rNFA = NFA(
      start = accepting,
      accepting = Set(start),
      tfun = tfun_prime,
      nullTrans = Array.fill(tfun.length)(Set.empty)
    )
    new MutableDFA().apply(rNFA)
  }
}
