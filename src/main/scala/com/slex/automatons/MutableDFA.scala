package com.slex.automatons

import scala.collection.mutable
import scala.collection.mutable.{Queue, Map as MMap, Set as MSet}

class MutableDFA {
  val dfaStateMap: MMap[Set[Int], Int] = mutable.Map(Set.empty -> 0)
  val tfun: MMap[Int, MMap[Int, Int]] = mutable.Map().withDefaultValue(MMap.empty)
  val accepting: MSet[Int] = MSet.empty

  def apply(machine: NFA): DFA = {
    construct(machine)
    toDFA
  }

  private def addTrans(from: Int, on: Char, to: Int): Unit = {
      tfun(from).put(on, to)
  }

  private def epsilonClosure(machine: NFA, states: Set[Int]): Set[Int] = {
    val q = mutable.Queue[Int]()
    val ret = mutable.Set[Int]()

    states foreach { s =>
      q enqueue s
      ret add s
    }

    while q.nonEmpty do {
      val elem = q.dequeue()
      ret add elem
      for i <- machine.nullTrans(elem) if !(ret contains i) do {
        q enqueue i
        ret add i
      }
    }
    ret.toSet
  }

  private def inputClosure(machine: NFA, states: Set[Int], input: Char): Set[Int] = {
    val eClosure = epsilonClosure(machine, states)
    val aClosure = eClosure.flatMap(state => machine.tfun(state)(input))
    epsilonClosure(machine, aClosure)
  }

  // There is currently a problem with
  private def construct(machine: NFA): Unit = {
    val todo = mutable.Queue[Set[Int]]()
    val closure = epsilonClosure(machine, machine.start)

    todo enqueue closure
    dfaStateMap.put(closure, dfaStateMap.size)

    while todo.nonEmpty do {
      val curr = todo.dequeue
      val currId = dfaStateMap(curr)

      val stateTrans = mutable.Map[Int, Int]()

      for symbol <- 0 until 128 do {
        val nextState = curr.flatMap(s => {
          machine.tfun(s).getOrElse(symbol, Set.empty)
        })

        val nextClose = epsilonClosure(machine, nextState)

        if nextClose.nonEmpty then {
          val destinationId = dfaStateMap.getOrElseUpdate(nextClose, {
            todo enqueue nextClose
            dfaStateMap.size
          })

          stateTrans.put(symbol, destinationId)
        }
      }

      tfun.put(currId, stateTrans)
    }

    for (nfaState, id) <- dfaStateMap.toList; if nfaState.exists(machine.accepting.contains) do {
      accepting add id
    }
  }

  private def toDFA: DFA = DFA(
      start = 1,
      accepting = accepting.toSet,
      tfun = (0 to tfun.size).map(state => {
        tfun.getOrElse(state, MMap.empty).toMap.withDefaultValue(0)
      }).toArray
    )
}
