package ir.slicer

import ir.*

type SlicingParameter = Variable | StackVariable | GlobalVariable

type StatementSlice = Set[SlicingParameter]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[SlicingParameter]
}
