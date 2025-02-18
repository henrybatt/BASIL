package ir.slicer

import ir.*

import ir.transforms.worklistSolver
import ir.transforms.AbstractDomain
import util.functional.State
import analysis.IntraProcConstantPropagation
import analysis.InterProcConstantPropagation
import analysis.FlatElement
import analysis.evaluateExpression
import analysis.getMemoryVariable
import ir.eval.BasilValue.add

private type SlicingParameter = Variable | StackVariable | GlobalVariable

private type StatementSlice = Set[SlicingParameter]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[SlicingParameter]
}

private trait SliceState:
  val entrySet = StatementSlice()
  val exitSet = StatementSlice()

private class BlockState extends SliceState

private class ProcedureState extends SliceState:
  val blocks = Map[Block, BlockState]()

case class StackPointer(variable: Set[Variable], operation: BinOp, offset: BitVecLiteral) {}

case class StackVariable(mem: Memory, index: StackPointer, endian: Endian, size: Int) {
  override def toString(): String = s"${index.variable.head.name} + ${index.offset}"
}

case class GlobalVariable(mem: Memory, address: BitVecLiteral, endian: Endian, size: Int, identifier: Option[String]) {
  override def toString(): String = s"${address}"
}

class SlicerDomain(
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  globals: Map[BigInt, String] = Map()
) extends AbstractDomain[StatementSlice] {
  def join(a: StatementSlice, b: StatementSlice, pos: Block) = a.union(b)

  def top = ???
  def bot = StatementSlice()

  private def convert(
    prop: Map[Variable, FlatElement[BitVecLiteral]],
    mem: Memory,
    expression: Expr,
    endian: Endian,
    size: Int
  ) =
    expression match {
      case BinaryExpr(op, arg1, arg2) =>
        val sp = StackPointer(arg1.variables, op, evaluateExpression(arg2, prop).get)
        val sv = StackVariable(mem, sp, endian, size)
        Set(sv)

      case o =>
        evaluateExpression(o, prop) match {
          case Some(addr) =>
            val gv = GlobalVariable(mem, addr, endian, size, globals.get(addr.value))
            Set(gv)
          case None =>
            Set() ++ expression.variables
        }
    }

  private def convertMemoryStore(a: MemoryStore): Set[SlicingParameter] =
    convert(constProp(a), a.mem, a.index, a.endian, a.size)

  def transfer(s: StatementSlice, a: Command): StatementSlice =
    a match {
      case a: LocalAssign => (s - a.lhs) ++ a.rhs.variables
      case a: MemoryLoad => (s - a.lhs) ++ convert(constProp(a), a.mem, a.index, a.endian, a.size)
      case m: MemoryStore =>
        (s -- convert(constProp(m), m.mem, m.index, m.endian, m.size)) ++ m.value.variables
      case a: Assume => s ++ a.body.variables
      case a: Assert => s ++ a.body.variables
      case _ => s
    }
}

class Slicer(program: Program):

  def run(): Unit =
    val constPropResults = InterProcConstantPropagation(program).analyze()

    val startingProc = program.mainProcedure
    val domain = SlicerDomain(constPropResults)

    val (first, last) = worklistSolver(domain).solveProc(startingProc, backwards = true)

    last.foreach { case (k, v) =>
      println(s"${k.label}\n\t${v.size}: $v")
    }
