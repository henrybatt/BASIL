package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import util.intrusive_list.IntrusiveList

abstract class Visitor {

  def visitExpr(node: Expr): Expr = node.acceptVisit(this)

  def visitStatement(node: Statement): Statement = node.acceptVisit(this)

  def visitMemoryAssign(node: MemoryAssign): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.rhs = visitExpr(node.rhs)
    node
  }

  def visitLocalAssign(node: LocalAssign): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.rhs = visitExpr(node.rhs)
    node
  }

  def visitMemoryStore(node: MemoryStore): Statement = {
    node.mem = visitMemory(node.mem)
    node.index = visitExpr(node.index)
    node.value = visitExpr(node.value)
    node
  }

  def visitMemoryLoad(node: MemoryLoad): Statement = {
    node.lhs = visitVariable(node.lhs)
    node.mem = visitMemory(node.mem)
    node.index = visitExpr(node.index)
    node
  }

  def visitAssume(node: Assume): Statement = {
    node.body = visitExpr(node.body)
    node
  }

  def visitAssert(node: Assert): Statement = {
    node.body = visitExpr(node.body)
    node
  }

  def visitJump(node: Jump): Jump = node.acceptVisit(this)

  def visitGoTo(node: GoTo): Jump = {
    node
  }

  def visitDirectCall(node: DirectCall): Statement = {
    val ins = node.actualParams.map(i => i._1 -> visitExpr(i._2))
    val outs = node.outParams.map(i => i._1 -> visitVariable(i._2))
    DirectCall(node.target, node.label, outs, ins)
  }

  def visitIndirectCall(node: IndirectCall): Statement = {
    node.target = visitVariable(node.target)
    node
  }

  def visitBlock(node: Block): Block = {
    for (s <- node.statements) {
      node.statements.replace(s, visitStatement(s))
    }
    node.replaceJump(visitJump(node.jump))
    node
  }

  def visitProcedure(node: Procedure): Procedure = {
    for (b <- node.blocks) {
      node.replaceBlock(b, visitBlock(b))
    }
    node.formalInParam = node.formalInParam.map(visitLocalVar)
    node.formalOutParam = node.formalOutParam.map(visitLocalVar)
    node
  }

  def visitProgram(node: Program): Program = {
    for (i <- node.procedures.indices) {
      val updatedProcedure = visitProcedure(node.procedures(i))
      val targetProcedure = node.procedures(i)
      if (targetProcedure == node.mainProcedure) {
        node.mainProcedure = updatedProcedure
      }
      node.procedures(i) = updatedProcedure
    }
    node
  }

  def visitExtract(node: Extract): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitRepeat(node: Repeat): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitZeroExtend(node: ZeroExtend): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitSignExtend(node: SignExtend): Expr = {
    node.copy(body = visitExpr(node.body))
  }

  def visitUnaryExpr(node: UnaryExpr): Expr = {
    node.copy(arg = visitExpr(node.arg))
  }

  def visitBinaryExpr(node: BinaryExpr): Expr = {
    node.copy(arg1 = visitExpr(node.arg1), arg2 = visitExpr(node.arg2))
  }

  def visitMemory(node: Memory): Memory = node.acceptVisit(this)

  def visitStackMemory(node: StackMemory): Memory = node

  def visitSharedMemory(node: SharedMemory): Memory = node

  def visitVariable(node: Variable): Variable = node.acceptVisit(this)

  def visitRegister(node: Register): Register = node

  def visitLocalVar(node: LocalVar): LocalVar = node

  def visitLiteral(node: Literal): Literal = node

  def visitUninterpretedFunction(node: UninterpretedFunction): UninterpretedFunction = {
    node.copy(params = node.params.map(visitExpr))
  }

}

abstract class ReadOnlyVisitor extends Visitor {
  override def visitExtract(node: Extract): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitRepeat(node: Repeat): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitZeroExtend(node: ZeroExtend): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitSignExtend(node: SignExtend): Expr = {
    visitExpr(node.body)
    node
  }

  override def visitUnaryExpr(node: UnaryExpr): Expr = {
    visitExpr(node.arg)
    node
  }

  override def visitBinaryExpr(node: BinaryExpr): Expr = {
    visitExpr(node.arg1)
    visitExpr(node.arg2)
    node
  }

  override def visitLocalAssign(node: LocalAssign): Statement = {
    visitVariable(node.lhs)
    visitExpr(node.rhs)
    node
  }

  override def visitMemoryStore(node: MemoryStore): Statement = {
    visitMemory(node.mem)
    visitExpr(node.index)
    visitExpr(node.value)
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): Statement = {
    visitVariable(node.lhs)
    visitMemory(node.mem)
    visitExpr(node.index)
    node
  }

  override def visitAssume(node: Assume): Statement = {
    visitExpr(node.body)
    node
  }

  override def visitAssert(node: Assert): Statement = {
    visitExpr(node.body)
    node
  }

  override def visitGoTo(node: GoTo): Jump = {
    node
  }

  override def visitDirectCall(node: DirectCall): Statement = {
    node.actualParams.foreach(i => visitExpr(i._2))
    node.outParams.foreach(i => visitVariable(i._2))
    node
  }

  override def visitIndirectCall(node: IndirectCall): Statement = {
    visitVariable(node.target)
    node
  }

  override def visitBlock(node: Block): Block = {
    for (i <- node.statements) {
      visitStatement(i)
    }
    visitJump(node.jump)
    node
  }

  override def visitProcedure(node: Procedure): Procedure = {
    for (i <- node.blocks) {
      visitBlock(i)
    }
    for (i <- node.formalInParam) {
      visitLocalVar(i)
    }
    for (i <- node.formalOutParam) {
      visitLocalVar(i)
    }
    node
  }

  override def visitProgram(node: Program): Program = {
    for (i <- node.procedures) {
      visitProcedure(i)
    }
    node
  }

  override def visitUninterpretedFunction(node: UninterpretedFunction): UninterpretedFunction = {
    for (i <- node.params) {
      visitExpr(i)
    }
    node
  }

}

/** Visits all reachable blocks in a procedure, depth-first, in the order they are reachable from the start of the
  * procedure. Does not jump to other procedures. Only modifies statements and jumps.
  */
abstract class IntraproceduralControlFlowVisitor extends Visitor {
  private val visitedBlocks: mutable.Set[Block] = mutable.Set()

  override def visitProcedure(node: Procedure): Procedure = {
    node.entryBlock.foreach(visitBlock)
    node
  }

  override def visitBlock(node: Block): Block = {
    if (visitedBlocks.contains(node)) {
      return node
    }
    for (i <- node.statements) {
      visitStatement(i)
    }
    visitedBlocks.add(node)
    node.replaceJump(visitJump(node.jump))
    node
  }

  override def visitGoTo(node: GoTo): Jump = {
    node.targets.foreach(visitBlock)
    node
  }

  override def visitDirectCall(node: DirectCall): Statement = {
    node
  }

  override def visitIndirectCall(node: IndirectCall): Statement = {
    node.target = visitVariable(node.target)
    node
  }
}

// TODO: does this break for programs with loops? need to calculate a fixed-point?
class StackSubstituter extends IntraproceduralControlFlowVisitor {
  private val stackPointer = Register("R31", 64)
  private val stackMemory = StackMemory("stack", 64, 8)
  val stackRefs: mutable.Set[Variable] = mutable.Set(stackPointer)

  override def visitProcedure(node: Procedure): Procedure = {
    // reset for each procedure
    stackRefs.clear()
    stackRefs.add(stackPointer)
    stackRefs.add(LocalVar("R31_in", BitVecType(64)))
    stackRefs.add(LocalVar("R31", BitVecType(64)))
    super.visitProcedure(node)
  }

  def isStackPtr(v: Variable) = {
    (v match {
      case l: LocalVar if l.varName == "R31" => true
      case r: Variable if r.name == "R31" => true
      case _ => false
    }) || stackRefs.contains(v)
  }

  override def visitMemoryLoad(node: MemoryLoad): MemoryLoad = {
    // replace mem with stack in load if index contains stack references
    if (node.index.variables.exists(isStackPtr)) {
      node.mem = stackMemory
    }

    if (stackRefs.contains(node.lhs) && node.lhs != stackPointer) {
      stackRefs.remove(node.lhs)
    }

    node
  }

  override def visitLocalAssign(node: LocalAssign): Statement = {
    // update stack references
    val variableVisitor = VariablesWithoutStoresLoads()
    variableVisitor.visitExpr(node.rhs)

    if (variableVisitor.variables.exists(isStackPtr)) {
      stackRefs.add(node.lhs)
    } else if (stackRefs.contains(node.lhs) && node.lhs.name != stackPointer.name) {
      stackRefs.remove(node.lhs)
    }
    node
  }

  override def visitMemoryStore(node: MemoryStore): Statement = {
    if (node.index.variables.exists(isStackPtr)) {
      node.mem = stackMemory
    }
    node
  }

}

class Substituter(variables: Map[Variable, Variable] = Map(), memories: Map[Memory, Memory] = Map()) extends Visitor {
  override def visitVariable(node: Variable): Variable = variables.get(node) match {
    case Some(v: Variable) => v
    case None => node
  }

  override def visitMemory(node: Memory): Memory = memories.get(node) match {
    case Some(m: Memory) => m
    case None => node
  }
}

/** Prevents strings in 'reserved' from being used as the name of anything by adding a '#' to the start. Useful for
  * avoiding Boogie's reserved keywords.
  */
class Renamer(reserved: Set[String]) extends Visitor {
  override def visitProgram(node: Program): Program = {
    for (section <- node.usedMemory.values) {
      section.region match {
        case Some(region) if reserved.contains(region.name) =>
          region.name = s"#${region.name}"
        case _ =>
      }
    }

    super.visitProgram(node)
  }

  override def visitLocalVar(node: LocalVar): LocalVar = {
    if (reserved.contains(node.varName)) {
      node.copy(varName = s"#${node.varName}")
    } else {
      node
    }
  }

  override def visitStackMemory(node: StackMemory): StackMemory = {
    if (reserved.contains(node.name)) {
      node.copy(name = s"#${node.name}")
    } else {
      node
    }
  }

  override def visitSharedMemory(node: SharedMemory): SharedMemory = {
    if (reserved.contains(node.name)) {
      node.copy(name = s"#${node.name}")
    } else {
      node
    }
  }

  override def visitProcedure(node: Procedure): Procedure = {
    if (reserved.contains(node.procName)) {
      node.procName = s"#${node.procName}"
    }
    super.visitProcedure(node)
  }

}

class ExternalRemover(external: Set[String]) extends Visitor {
  override def visitProcedure(node: Procedure): Procedure = {
    if (external.contains(node.procName)) {
      // update the modifies set before removing the body
      node.modifies.addAll(node.blocks.flatMap(_.modifies))
      node.replaceBlocks(Seq())
    }
    super.visitProcedure(node)
  }
}

/** Gives variables that are not contained within a MemoryStore or the rhs of a MemoryLoad
  */
class VariablesWithoutStoresLoads extends ReadOnlyVisitor {
  val variables: mutable.Set[Variable] = mutable.Set()

  override def visitRegister(node: Register): Register = {
    variables.add(node)
    node
  }

  override def visitLocalVar(node: LocalVar): LocalVar = {
    variables.add(node)
    node
  }

  override def visitMemoryLoad(node: MemoryLoad): MemoryLoad = {
    visitVariable(node.lhs)
    node
  }

}
