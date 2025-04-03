package ir.slicer

import ir.*
import ir.eval.evaluateExpr
import analysis.RangeKey

type AddressLiteral = BitVecLiteral | IntLiteral

case class StackVariable(mem: Memory, variable: Variable, offset: AddressLiteral) {
  override def toString(): String = s"StackVariable($mem, $variable, $offset)"
}

case class GlobalVariable(mem: Memory, address: AddressLiteral, identifier: String) {
  override def toString(): String = s"GlobalVariable($mem, $identifier, $address)"
}

case class TestVariable(id: String) {
  override def toString(): String = s"TestVariable(${id})"
}

def convert(mem: Memory, expression: Expr, globals: Map[RangeKey, String]): Option[SlicingParameter] = {

  def evalStackVar(arg1: Variable, arg2: Expr): Option[SlicingParameter] = {
    evaluateExpr(arg2) match {
      case Some(literal: AddressLiteral) => Some(StackVariable(mem, arg1, literal))
      case _ => None
    }
  }

  def evalGlobalVar(e: Expr): Option[SlicingParameter] = {
    evaluateExpr(e) match {
      case Some(addr: BitVecLiteral) => {
        globals.find((range, _) => range.start <= addr.value && addr.value <= range.end) match {
          case Some(_, identifier) => Some(GlobalVariable(mem, addr, identifier))
          case None => None
        }
      }
      case Some(addr: IntLiteral) => {
        globals.find((range, _) => range.start <= addr.value && addr.value <= range.end) match {
          case Some(_, identifier) => Some(GlobalVariable(mem, addr, identifier))
          case None => None
        }
      }
      case _ => None
    }
  }

  expression match {
    case BinaryExpr(BVADD, arg1: Variable, arg2) => evalStackVar(arg1, arg2)
    case BinaryExpr(BVSUB, arg1: Variable, arg2) => evalStackVar(arg1, UnaryExpr(BVNEG, arg2))
    case v: Variable => Some(v)
    case e => evalGlobalVar(e)
  }
}
