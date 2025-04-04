package ir.slicer

import ir.*
import boogie.*
import analysis.RangeKey
import analysis.*

import analysis.solvers.*

trait SliceAnalysisFunctions(globals: Map[RangeKey, String])
    extends BackwardIDEAnalysis[SlicingParameter, TwoElement, TwoElementLattice] {
  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    // println(s"CallToEntry:\n\t$call\n\t$entry\n")
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    // println(s"ExitAfterCall:\n\t$exit\n\t$aftercall\n")
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    // println(s"CallAfterCall:\n\t$call\n\t$aftercall\n")
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match {
      case p: Procedure => Map(d -> IdEdge())
      case b: Block => Map(d -> IdEdge())
      case a: LocalAssign => {
        d match {
          case Left(value) if value == a.lhs =>
            a.rhs.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (mp, expVar) =>
              mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
          case _ => Map(d -> IdEdge())
        }
      }
      case a: MemoryLoad => {
        d match {
          case Left(value) if value == a.lhs =>
            convert(a.mem, a.index, globals) match {
              case Some(variable) => Map(Left(variable) -> IdEdge())
              case None => {
                a.index.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (mp, expVar) =>
                  mp + (Left(expVar) -> ConstEdge(TwoElementTop))
                }
              }
            }
          case _ => Map(d -> IdEdge())
        }
      }
      case m: MemoryStore => {
        d match {
          case Left(value) =>
            convert(m.mem, m.index, globals) match {
              case Some(variable) if value == variable =>
                m.value.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (mp, expVar) =>
                  mp + (Left(expVar) -> ConstEdge(TwoElementTop))
                }
              case Some(variable) => Map(d -> IdEdge())
              case None =>
                value match {
                  case v: Variable if m.index.variables.contains(v) =>
                    m.value.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (mp, expVar) =>
                      mp + (Left(expVar) -> ConstEdge(TwoElementTop))
                    }
                  case _ => Map(d -> IdEdge())
                }
            }
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case a: Assume => {
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            a.body.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) { (mp, expVar) =>
              mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      }
      case a: Assert => {
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            a.body.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) { (mp, expVar) =>
              mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      }
      case i: IndirectCall => Map(d -> IdEdge())
      case c: DirectCall => Map(d -> IdEdge())
      case g: GoTo => Map(d -> IdEdge())
      case r: Return => Map(d -> IdEdge())
      case u: Unreachable => Map(d -> IdEdge())
      case n: NOP => Map(d -> IdEdge())
      // case _ => Map(d -> IdEdge())
    }
  }
}

class SliceAnalysis(program: Program, globals: Map[RangeKey, String])
    extends BackwardIDESolver[SlicingParameter, TwoElement, TwoElementLattice](program),
      SliceAnalysisFunctions(globals)
