package ir.slicer

import ir.*

import ir.transforms.worklistSolver
import ir.transforms.AbstractDomain
import scala.collection.mutable.Stack

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
) extends AbstractDomain[StatementSlice] {
  def join(a: StatementSlice, b: StatementSlice, pos: Block) = a.union(b)

  def top = ???
  def bot = StatementSlice()

  def transfer(s: StatementSlice, a: Command): StatementSlice =
    a match {
      case a: LocalAssign => (s - a.lhs) ++ a.rhs.variables
      case a: MemoryLoad => (s - a.lhs) ++ a.index.variables
      case m: MemoryStore => s ++ m.index.variables ++ m.value.variables
      case a: Assume => s ++ a.body.variables
      case a: Assert => s ++ a.body.variables
      case _ => s
    }
}

class Slicer(program: Program):

  def run(): Unit =
    val startingProc = program.mainProcedure
    val domain = SlicerDomain()

    val (first, last) = worklistSolver(domain).solveProc(startingProc, backwards = true)

    last.foreach { case (k, v) =>
      println(s"${k.label}\n\t${v.size}: $v")
    }
