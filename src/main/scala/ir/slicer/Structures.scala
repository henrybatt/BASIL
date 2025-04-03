package ir.slicer

import ir.*

type SlicingParameter = Variable | StackVariable | GlobalVariable | TestVariable

type StatementSlice = Set[SlicingParameter]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[SlicingParameter]
}
