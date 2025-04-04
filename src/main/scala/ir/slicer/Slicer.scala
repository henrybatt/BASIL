package ir.slicer

import ir.*
import ir.eval.evaluateExpr

import boogie.*
import analysis.RangeKey
import util.SlicerLogger

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

  private def setsToString(
    entrySets: Map[CFGPosition, StatementSlice],
    exitSets: Map[CFGPosition, StatementSlice]
  ): String = {

    def get(n: CFGPosition, indent: String = ""): String = {
      s"${indent}> Entry: ${entrySets.getOrElse(n, StatementSlice())}\n${indent}> Exit:  ${exitSets.getOrElse(n, StatementSlice())}"
    }

    var result = ""
    for (proc <- program.procedures) {
      result += (s"-------Proc: ${proc.name}-------") + "\n"
      result += get(proc) + "\n\n"
      var i = 1;
      for (block <- proc.blocks) {
        result += (s"\t------$i Block: ${block.label}-------") + "\n"
        result += get(block, "\t") + "\n\n"
        i += 1
        for (statement <- block.statements) {
          result += (s"\t\t$statement") + "\n"
          result += get(block, "\t\t") + "\n\n"
        }
        result += (s"\t\t${block.jump}") + "\n"
        result += get(block.jump, "\t\t") + "\n\n"
      }
    }
    result
  }

  private def computeEntrySets(
    summary: Map[CFGPosition, StatementSlice],
    slicingCriterion: Map[CFGPosition, StatementSlice] = Map()
  ): Map[CFGPosition, StatementSlice] = {

    def flatten(n: Iterable[Procedure | Block]): StatementSlice = {
      n.toList.flatMap(b => summary.get(b).toList) match {
        case Nil => Set.empty
        case h :: Nil => h
        case h :: tl => tl.foldLeft(h)((acc, nb) => acc.union(nb))
      }
    }

    def entrySet(n: CFGPosition): StatementSlice = {
      (n match {
        case p: Procedure => flatten(p.callers())
        case b: Block => flatten(b.nextBlocks)
        case s: Statement => summary(s.successor)
        case _ => StatementSlice()
      }) ++ slicingCriterion.getOrElse(n, StatementSlice())

    }

    summary.map({ case (k, v) => (k, entrySet(k)) })
  }

  def run(): Unit = {

    val slicingCriterion: Map[CFGPosition, StatementSlice] = Map(
    )

    val results = SliceAnalysis(program, transformGlobals())
      .analyze()
      .map({ case (k, v) =>
        (k -> v.keys.toSet)
      })

    val entrySets = computeEntrySets(results, slicingCriterion)
    SlicerLogger.info(setsToString(entrySets, results))
  }
}
