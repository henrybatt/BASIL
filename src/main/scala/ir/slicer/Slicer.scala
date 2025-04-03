package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import ir.transforms.*
import analysis.IntraProcConstantPropagation
import analysis.InterProcConstantPropagation
import analysis.{evaluateExpression, FlatElement}
import boogie.*
import analysis.RangeKey

private def convert(mem: Memory, expression: Expr, globals: Map[RangeKey, String]): Option[SlicingParameter] =

  def evalStackVar(arg1: Variable, arg2: Expr): Option[SlicingParameter] =
    evaluateExpr(arg2) match {
      case Some(literal: (BitVecLiteral | IntLiteral)) => Some(StackVariable(mem, arg1, literal))
      case _ => None
    }

  def evalGlobalVar(e: Expr): Option[SlicingParameter] =
    evaluateExpr(e) match {
      case Some(addr: BitVecLiteral) =>
        globals.find((range, _) => range.start <= addr.value && addr.value <= range.end) match {
          case Some(_, identifier) => Some(GlobalVariable(mem, addr, identifier))
          case None => None
        }
      case _ => None
    }

  expression match {
    case BinaryExpr(BVADD, arg1: Variable, arg2) => evalStackVar(arg1, arg2)
    case BinaryExpr(BVSUB, arg1: Variable, arg2) => evalStackVar(arg1, UnaryExpr(BVNEG, arg2))
    case v: Variable => Some(v)
    case e => evalGlobalVar(e)
  }

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

