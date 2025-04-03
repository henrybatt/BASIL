package ir.slicer

import ir.Program
import util.SlicerLogger

object Other {
  def print_program(program: Program): Unit = {
    SlicerLogger.info(print_program(program))
  }

  def program_toString(program: Program): String = {
    var result = ""
    for (proc <- program.procedures) {
      result += (s"-------Proc: ${proc.name}-------") + "\n"
      var i = 1;
      for (block <- proc.blocks) {
        result += (s"\t------$i Block: ${block.label}-------") + "\n"
        i += 1
        for (statement <- block.statements) {
          result += (s"\t\t$statement\n") + "\n"
        }
        result += (s"\t\t${block.jump}\n") + "\n"
      }
    }
    result
  }

}
