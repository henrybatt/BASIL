package ir.slicer

import ir.*

case class StackVariable(mem: Memory, variable: Variable, offset: BitVecLiteral | IntLiteral) {
  override def toString(): String = s"StackVariable($mem, $variable, $offset)"
}

case class GlobalVariable(mem: Memory, address: BitVecLiteral | IntLiteral, identifier: String) {
  override def toString(): String = s"GlobalVariable($mem, $identifier, $address)"
}
