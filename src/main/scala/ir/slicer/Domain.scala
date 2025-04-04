package ir.slicer

import ir.*
import analysis.RangeKey
import analysis.solvers.SimpleWorklistFixpointSolver
import analysis.*
import analysis.solvers.*

abstract class TestAnalysis(
  program: Program,
  globals: Map[RangeKey, String] = Map(),
  slicingCriterion: Map[CFGPosition, StatementSlice] = Map()
) extends Analysis[Any] {
  val lattice: MapLattice[CFGPosition, StatementSlice, PowersetLattice[SlicingParameter]] = MapLattice(
    PowersetLattice()
  )
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: StatementSlice): StatementSlice =
    val extended = s ++ slicingCriterion.getOrElse(n, StatementSlice())

    n match {
      case p: Procedure => extended
      case b: Block => extended
      case a: LocalAssign =>
        if (extended.contains(a.lhs)) then (extended - a.lhs) ++ a.rhs.variables
        else extended

      case a: MemoryLoad =>
        if (extended.contains(a.lhs)) then
          convert(a.mem, a.index, globals) match {
            case Some(variable) => (extended - a.lhs) + variable
            case None => (extended - a.lhs) ++ a.index.variables
          }
        else extended

      case m: MemoryStore =>
        convert(m.mem, m.index, globals) match {
          case Some(variable) =>
            if (extended.contains(variable)) then (extended - variable) ++ m.value.variables else extended
          case None =>
            if ((Set() ++ m.index.variables).subsetOf(extended)) then
              (extended -- m.index.variables) ++ m.value.variables
            else extended
        }

      case a: Assume => extended ++ a.body.variables
      case a: Assert => extended ++ a.body.variables
      case i: IndirectCall => extended
      case c: DirectCall => extended
      case g: GoTo => extended
      case r: Return => extended
      case u: Unreachable => extended
      case n: NOP => extended
    }
}

class SlicerAnalysis(
  program: Program,
  globals: Map[RangeKey, String] = Map(),
  slicingCriterion: Map[CFGPosition, StatementSlice] = Map()
) extends TestAnalysis(program, globals, slicingCriterion)
    with SimpleWorklistFixpointSolver[CFGPosition, StatementSlice, PowersetLattice[SlicingParameter]]
    with IRIntraproceduralBackwardDependencies
