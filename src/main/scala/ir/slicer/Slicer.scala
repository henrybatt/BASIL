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

  def run(): Unit = {

    val slicingCriterion: Map[CFGPosition, StatementSlice] = Map(
    )

    val results = SlicerAnalysis(program, globals = transformGlobals(), slicingCriterion = slicingCriterion).analyze()


