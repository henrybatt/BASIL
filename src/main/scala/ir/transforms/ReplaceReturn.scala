package ir.transforms

import util.Logger
import ir.cilvisitor.*
import ir.*

class ReplaceReturns(insertR30InvariantAssertion: Procedure => Boolean = (_ => true)) extends CILVisitor {

  /** Assumes IR with 1 call per block which appears as the last statement.
    */
  override def vstmt(j: Statement): VisitAction[List[Statement]] = {
    val assertR30Addr = insertR30InvariantAssertion(j.parent.parent)

    j match {
      case IndirectCall(r30 @ Register("R30", rt), _) => {
        assert(j.parent.statements.lastOption.contains(j))
        if (j.parent.jump.isInstanceOf[Unreachable | Return]) {
          j.parent.replaceJump(Return())
          val R30Begin = LocalVar("R30_begin", BitVecType(64))
          if (assertR30Addr) {
            ChangeTo(List(Assert(BinaryExpr(EQ, r30, R30Begin), Some("is returning to caller-set R30"))))
          } else {
            ChangeTo(List())
          }
        } else {
          SkipChildren()
        }
      }
      case i: IndirectCall => {
        (i.predecessor, i.parent.jump) match {
          case (Some(l: LocalAssign), _) if l.lhs.name == "R30" => SkipChildren()
          case (_, _) => {
            val R30Begin = LocalVar("R30_begin", BitVecType(64))
            i.parent.replaceJump(Return())
            if (assertR30Addr) {
              ChangeTo(List(Assert(BinaryExpr(EQ, Register("R30", 64), R30Begin), Some("R30 = R30_in")), i))
            } else {
              SkipChildren()
            }
          }
        }
      }
      case d: DirectCall => {
        (d.predecessor, d.parent.jump) match {
          // d.parent.jump == d.successor,  from singleprocend invariant
          // case (Some(l: LocalAssign), _) if l.lhs.name == "R30" && l.rhs.isInstanceOf[BitVecLiteral] => SkipChildren()
          // ^ we can resolve the exact return target if we are assigning a constant
          // If we can't find one
          case (_, _: Unreachable) if d.target == d.parent.parent => {
            // recursive tailcall
            val R30Begin = LocalVar("R30_begin", BitVecType(64))
            d.parent.replaceJump(GoTo((d.parent.parent.entryBlock.get)))
            if (assertR30Addr) {
              ChangeTo(List(Assert(BinaryExpr(EQ, Register("R30", 64), R30Begin), Some("R30 = R30_in")), d))
            } else {
              SkipChildren()
            }
          }
          case (_, _: Unreachable) => {
            val R30Begin = LocalVar("R30_begin", BitVecType(64))
            d.parent.replaceJump(Return())
            if (assertR30Addr) {
              ChangeTo(List(Assert(BinaryExpr(EQ, Register("R30", 64), R30Begin), Some("R30 = R30_in")), d))
            } else {
              SkipChildren()
            }
          }
          case _ => SkipChildren()
        }

      }
      case _ => SkipChildren()
    }
  }

  override def vjump(j: Jump) = SkipChildren()
}

def addReturnBlocks(
  p: Program,
  toAll: Boolean = false,
  insertR30InvariantAssertion: Procedure => Boolean = _ => false
) = {
  p.procedures.foreach(p => {
    val containsReturn = p.blocks.map(_.jump).find(_.isInstanceOf[Return]).isDefined
    if (toAll && p.blocks.isEmpty && p.entryBlock.isEmpty && p.returnBlock.isEmpty) {
      p.returnBlock = (Block(label = p.name + "_basil_return", jump = Return()))
      p.entryBlock = (Block(label = p.name + "_basil_entry", jump = GoTo(p.returnBlock.get)))
    } else if (p.returnBlock.isEmpty && (toAll || containsReturn)) {
      p.returnBlock = p.addBlock(Block(label = p.name + "_basil_return", jump = Return()))
    }
    if (insertR30InvariantAssertion(p)) {
      for (eb <- p.entryBlock) {
        val R30Begin = LocalVar("R30_begin", BitVecType(64))
        p.entryBlock.get.statements.prepend(LocalAssign(R30Begin, Register("R30", 64)))
      }
    }
  })
}

class ConvertSingleReturn extends CILVisitor {

  /** Assumes procedures have defined return blocks if they contain a return statement.
    */
  override def vjump(j: Jump) = j match {
    case r: Return if !(j.parent.parent.returnBlock.contains(j.parent)) =>
      ChangeTo(GoTo(Seq(j.parent.parent.returnBlock.get)))
    case _ => SkipChildren()
  }

  override def vstmt(s: Statement) = SkipChildren()
}
