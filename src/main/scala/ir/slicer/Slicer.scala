package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.*
import analysis.RangeKey


class SlicerDomain(globals: Map[RangeKey, String])
    extends PowerSetDomain[SlicingParameter] {

  def transfer(s: StatementSlice, a: Command): StatementSlice =
    a match {
      case a: LocalAssign =>
        if (s.contains(a.lhs)) then (s - a.lhs) ++ a.rhs.variables
        else s

      case a: MemoryLoad =>
        convert(a.mem, a.index, globals)
        if (s.contains(a.lhs)) then
          convert(a.mem, a.index, globals) match {
            case Some(variable) => (s - a.lhs) + variable
            case None => (s - a.lhs) ++ a.index.variables
          }
        else s

      case m: MemoryStore =>
        convert(m.mem, m.index, globals) match {
          case Some(variable) => if (s.contains(variable)) then (s - variable) ++ m.value.variables else s
          case None =>
            if ((Set() ++ m.index.variables).subsetOf(s)) then (s -- m.index.variables) ++ m.value.variables
            else s
        }

      case a: Assume => s ++ a.body.variables
      case a: Assert => s ++ a.body.variables
      case i: IndirectCall => s
      case c: DirectCall => s
      case g: GoTo => s
      case r: Return => s
      case r: Unreachable => s
      case n: NOP => s
    }
}

class Slicer(program: Program, globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt]):

  def transform_globals(): Map[RangeKey, String] =
    val global = globals
      .map(s => RangeKey(s.address, s.address + (s.size / BigInt(8)) * BigInt(s.arraySize.getOrElse(1)) - 1) -> s.name)
      .toMap
    global ++ globalOffsets.collect({
      case (k, v) if global.find((range, _) => range.start == v).nonEmpty =>
        (RangeKey(k, k), (global.find((range, _) => range.start == v).get).last)
    })

  def run(): Unit =
    val domain = SlicerDomain(
      globals = transform_globals(),
    )

    val (entrySet, exitSet) = worklistSolver(domain).solveProc(program.mainProcedure, backwards = true)

    // entrySet.foreach{
    //   case (k, v) => Logger.info(s"${k.label}\n\tEntry: ${entrySet(k).size}: ${exitSet(k)} ->\n\tExit: ${v.size}: $v")
    // }

