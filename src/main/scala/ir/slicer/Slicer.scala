package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.*
import analysis.RangeKey

class Slicer(program: Program, globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt]) {

  private def transformGlobals(): Map[RangeKey, String] = {
    val global = globals
      .map(s => RangeKey(s.address, s.address + (s.size / BigInt(8)) * BigInt(s.arraySize.getOrElse(1)) - 1) -> s.name)
      .toMap
    global ++ globalOffsets.collect({
      case (k, v) if global.find((range, _) => range.start == v).nonEmpty =>
        (RangeKey(k, k), (global.find((range, _) => range.start == v).get).last)
    })
  }

  private def computeEntrySets(
    summary: Map[CFGPosition, StatementSlice],
    slicingCriterion: Map[CFGPosition, StatementSlice] = Map()
  ): Map[CFGPosition, StatementSlice] = {

    def flattern(n: Iterable[Procedure | Block]): StatementSlice = {
      n.toList.flatMap(b => summary.get(b).toList) match {
        case Nil => Set.empty
        case h :: Nil => h
        case h :: tl => tl.foldLeft(h)((acc, nb) => acc.union(nb))
      }
    }

    def entrySet(n: CFGPosition): StatementSlice = {
      (n match {
        case p: Procedure => flattern(p.callers())
        case b: Block => flattern(b.nextBlocks)
        case s: Statement => summary(s.successor)
        case _ => StatementSlice()
      }) ++ slicingCriterion.getOrElse(n, StatementSlice())

    }

    summary.map({ case (k, v) => (k, entrySet(k)) })
  }

  def run(): Unit = {

    val slicingCriterion: Map[CFGPosition, StatementSlice] = Map(
    )

    val results = SlicerAnalysis(program, globals = transformGlobals(), slicingCriterion = slicingCriterion).analyze()

    val entrySets = computeEntrySets(results, slicingCriterion)

  }

}
