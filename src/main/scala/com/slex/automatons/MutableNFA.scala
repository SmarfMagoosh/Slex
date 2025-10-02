package com.slex.automatons

import com.slex.parsers.RegexAtom
import com.slex.parsers.RegexAtom.*

import scala.collection.mutable.{Map as MMap, Set as MSet}

class MutableNFA {
  var nextState: Int = 1
  val start: Int = 1
  val tfun: MMap[Int, MMap[Char, MSet[Int]]] = MMap(1 -> MMap.empty)
  val accepting: MSet[Int] = MSet.empty
  val nullTrans: MMap[Int, MSet[Int]] = MMap(1 -> MSet.empty)

  def apply(component: RegexAtom): NFA = {
    construct(component)
    toNFA
  }

  private def construct(atom: RegexAtom): Unit = {
    val endAt = addComponent(1, atom)
    initNextState(true)
    nullTrans(endAt).add(nextState)
  }

  private def initNextState(accepts: Boolean): Unit = {
    nextState = nextState + 1
    tfun.put(nextState, MMap.empty)
    nullTrans.put(nextState, MSet.empty)
    if accepts then accepting.add(nextState)
  }

  private def addTrans(from: Int, on: Char, to: Int): Unit = {
    if tfun(from) contains on then {
      tfun(from)(on).add(to)
    } else {
      tfun(from).put(on, MSet(to))
    }
  }

  /**
   * Recursively adds regex atoms to the NFA.
   * @param startAt The state that we start at when adding the atom
   * @param component the component we are adding
   * @return the state we end at after adding the component
   */
  private def addComponent(startAt: Int, component: RegexAtom): Int = {
    component match
      // desugar
      case CHAR(c) => addComponent(startAt, CLASS(Set(c)))
      case GROUP(atom) => addComponent(startAt, atom)
      case REPEATEXACT(atom, n) => addComponent(startAt, CONCAT(List.fill(n)(atom): _*))
      case REPEATATLEAST(atom, n) => addComponent(startAt, CONCAT(REPEATEXACT(atom, n), STAR(atom)))
      case REPEATBETWEEN(atom, n, m) =>
        val add = List.fill(n)(atom) ::: List.fill(m- n)(QMARK(atom))
        addComponent(startAt, CONCAT(add: _*))
      case PLUS(atom) => addComponent(startAt, CONCAT(atom, STAR(atom)))
      case CONCAT(atoms*) => atoms.foldLeft(startAt)((nextStart, atom) => addComponent(nextStart, atom))

      // actual implementation logic
      case ALTER(atoms*) =>
        val ends = atoms.map(comp => addComponent(startAt, comp))
        ends.tail.foreach(endPoint => nullTrans(endPoint).add(ends.head))
        ends.head
      case QMARK(atom) =>
        val endAt = addComponent(startAt, atom)
        nullTrans(startAt).add(endAt)
        endAt
      case STAR(atom) =>
        val endAt = addComponent(startAt, atom)
        nullTrans(startAt).add(endAt)
        nullTrans(endAt).add(startAt)
        endAt
      case CLASS(chars) =>
        initNextState(false)
        chars.foreach(c => {
          addTrans(startAt, c, nextState)
        })
        nextState
  }

  private def toNFA: NFA = {
    NFA(
      Set(start),
      accepting.toSet,
      (0 to nextState).map(state =>
        tfun.getOrElse(state, MMap.empty).map((k, v) => (k.toInt, v.toSet)).toMap.withDefaultValue(Set.empty)
      ).toArray,
      (0 to nextState).map(state => nullTrans.getOrElse(state, MSet.empty).toSet).toArray
    )
  }
}
