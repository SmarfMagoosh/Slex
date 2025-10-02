package com.slex.automatons

/**
 * @param start the set of starting states
 * @param accepting the set of accepting states
 * @param tfun the non-deterministic transition function
 * @param nulltrans the null transitions for the NFA
 */
class NFA(val start: Set[Int], val accepting: Set[Int], val tfun: Array[Map[Int, Set[Int]]], val nullTrans: Array[Set[Int]]) {
  def accepts(s: String): Boolean = {
    val end = s.foldLeft(start)(transition)
    accepting.exists(end.contains)
  }

  def transition(states: Set[Int], input: Char): Set[Int] = {
    states.flatMap(tfun(_)(input))
  }

  override def toString: String = {
    def getRow(i: Int): Map[Int, Set[Int]] = tfun(i).filter((k, v) => v.nonEmpty)
    def toChar(i: Int): String = i.toChar match {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\u000B' => "\\v" // vertical tab
      case ' ' => "\" \""
      case c if c.isWhitespace => "\"" + c + "\""
      case c => c.toString
    }

    s"""
       |Start States:
       |\t$start\n
       |Accepting States:
       |\t$accepting\n
       |Transition Function:
       |${tfun.indices.map(i => getRow(i).map((j, s) => s"\t($i, ${toChar(j)}) => $s\n").mkString).mkString}
       |Null Transitions:
       |${nullTrans.indices.map(i => s"\t$i => ${nullTrans(i)}\n").mkString}
       |""".stripMargin
  }
}


