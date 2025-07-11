package ir.parsing
import ir.Sigil

import basil_ir.{Absyn => syntax}

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

private object Declarations {
  lazy val empty = Declarations(Map(), Map(), Map(), Map(), SymbolTableInfo.empty, ProgSpec())
}

/**
 * Container for the result of the [[ir.parsing.BasilEarlyBNFCVisitor]].
 * Stores global variable declarations, memory region declarations, procedure signatures,
 * and metadata.
 *
 * Note that the [[ir.dsl.EventuallyProcedure]] structures stored by this class are
 * *incomplete*. Only the procedure name and the formalIn/Out parameters should be used.
 */
case class Declarations(
  val globals: Map[String, ir.Register],
  val functions: Map[String, FunDecl],
  val memories: Map[String, ir.Memory],
  val procedures: Map[String, ir.dsl.EventuallyProcedure],
  val symtab: SymbolTableInfo,
  val progSpec: ProgSpec
) {
  private def ensureDisjoint[T, U](x: Map[T, U], y: Map[T, U]): Unit =
    val overlap = x.keySet.intersect(y.keySet)
    if (!overlap.isEmpty) {
      throw new IllegalArgumentException(
        "invalid attempt to merge non-disjoint declarations. repeated names: " + overlap
      )
    }

  @throws[IllegalArgumentException]("if the two Declarations have overlapping names")
  def merge(other: Declarations) = {
    ensureDisjoint(globals, other.globals)
    ensureDisjoint(functions, other.functions)
    ensureDisjoint(memories, other.memories)
    ensureDisjoint(procedures, other.procedures)
    Declarations(
      globals ++ other.globals,
      functions ++ other.functions,
      memories ++ other.memories,
      procedures ++ other.procedures,
      symtab.merge(other.symtab),
      progSpec.merge(other.progSpec)
    )
  }
}

trait AttributeListBNFCVisitor[A]()
    extends syntax.AttrDefList.Visitor[Attrib.Map, A],
      syntax.AttrValue.Visitor[Attrib, A],
      syntax.AttrKeyValue.Visitor[(String, Attrib), A],
      LiteralsBNFCVisitor[A] {

  def visit(x: syntax.MapAttr, arg: A): ir.parsing.Attrib =
    Attrib.Map(ListMap.from(x.listattrkeyvalue_.asScala.toList.map(_.accept(this, arg))))
  def visit(x: syntax.ListAttr, arg: A): ir.parsing.Attrib =
    Attrib.List(x.listattrvalue_.asScala.toVector.map(_.accept(this, arg)))
  def visit(x: syntax.LiteralAttr, arg: A): ir.parsing.Attrib =
    Attrib.ValLiteral(x.value_.accept(this, arg))
  def visit(x: syntax.StringAttr, arg: A): ir.parsing.Attrib = {
    Attrib.ValString(unquote(x.str_, x))
  }

  override def visit(p: syntax.AttrDefListEmpty, arg: A): Attrib.Map = Attrib.Map(ListMap())
  override def visit(p: syntax.AttrKeyValue1, arg: A): (String, Attrib) = {
    (unsigilAttrib(p.bident_), p.attrvalue_.accept(this, arg))
  }
  override def visit(p: syntax.AttrDefListSome, arg: A): Attrib.Map = {
    Attrib.Map(ListMap.from(p.listattrkeyvalue_.asScala.toList.map(_.accept(this, arg))))
  }

  def getIntCompatAttr(n: String)(attrs: Attrib): Option[BigInt] = {
    attrs match {
      case Attrib.Map(vs) =>
        vs.collect {
          case (attr, Attrib.ValLiteral(ir.IntLiteral(v))) if attr == n => v
          case (attr, Attrib.ValLiteral(ir.BitVecLiteral(v, _))) if attr == n => v
        }.lastOption
      case _ => None
    }
  }

  def parseAttr(a: syntax.AttrValue, arg: A): Attrib = {
    a.accept(this, arg)
  }

  def parseAttrMap(a: syntax.AttrDefList, arg: A): Attrib = {
    a.accept(this, arg)
  }

  def getAddrAttr(attrs: Attrib): Option[BigInt] = getIntCompatAttr("address")(attrs)

  def getStrAttr(n: String)(attrs: Attrib): Option[String] = {
    attrs match {
      case Attrib.Map(vs) =>
        vs.collect {
          case (attr, Attrib.ValString(v)) if attr == n => v
        }.lastOption
      case _ => None
    }
  }
  val getCommentAttr = getStrAttr("comment")
  val getLabelAttr = getStrAttr("label")

}

/**
 * Performs an initial pass to read global declarations. Importantly, this picks up
 * procedure declarations and their paramter lists, so a later pass can correctly map a
 * direct call's actual arguments to their formal parameters.
 *
 * Visiting a [[basil_ir.Absyn.Prog]] with this visitor will return the complete declarations
 * object for the entire program. Using the other visit methods will only return declarations
 * for that subset of the AST.
 */
case class BasilEarlyBNFCVisitor[A]()
    extends syntax.Module.Visitor[Declarations, A],
      syntax.Declaration.Visitor[Declarations, A],
      syntax.ProcSig.Visitor[Declarations, A],
      syntax.Params.Visitor[(String, ir.IRType), A],
      TypesBNFCVisitor[A],
      LiteralsBNFCVisitor[A],
      AttributeListBNFCVisitor[A] {

  override def visit(x: syntax.AxiomDecl, arg: A) = ???

  override def visit(x: syntax.Module1, arg: A) =
    x.listdeclaration_.asScala.foldLeft(Declarations.empty) { case (decls, x) =>
      try {
        decls.merge(x.accept(this, arg))
      } catch {
        case e: IllegalArgumentException =>
          throw ParseException(
            "encountered duplicate declarations with the same name",
            x.asInstanceOf[HasParsePosition],
            e
          )
      }
    }

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.ProgDecl, arg: A) = Declarations.empty
  override def visit(x: syntax.ProgDeclWithSpec, arg: A) = Declarations.empty

  override def visit(x: syntax.UnsharedMemDecl, arg: A) =
    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = ir.StackMemory(unsigilGlobal(x.globalident_), addrwd, valwd)
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  override def visit(x: syntax.SharedMemDecl, arg: A) =
    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = ir.SharedMemory(unsigilGlobal(x.globalident_), addrwd, valwd)
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  def visit(x: basil_ir.Absyn.UninterpFunDecl, arg: A): ir.parsing.Declarations = {
    val n = unsigilGlobal(x.globalident_)
    val paramTypes = x.listtype_.asScala.toList.map(_.accept(this, arg))
    val returnType = x.type_.accept(this, arg)
    val ty = ir.uncurryFunctionType(paramTypes, returnType)
    Declarations.empty.copy(functions = Map(n -> FunDecl(ty, None)))
  }
  def visit(x: basil_ir.Absyn.FunDef, arg: A): ir.parsing.Declarations = {
    val n = unsigilGlobal(x.globalident_)
    val paramTypes = visitParams(x.listparams_, arg).toList.map(_._2)
    val returnType = x.type_.accept(this, arg)
    val ty = ir.uncurryFunctionType(paramTypes, returnType)
    Declarations.empty.copy(functions = Map(n -> FunDecl(ty, None)))
  }

  override def visit(x: syntax.VarDecl, arg: A) =
    val v = ir.Register(unsigilGlobal(x.globalident_), x.type_.accept(this, arg).asInstanceOf[ir.BitVecType].size)
    Declarations.empty.copy(globals = Map(v.name -> v))

  override def visit(x: syntax.Param, arg: A): (String, ir.IRType) =
    val lv = ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), x.type_.accept(this, arg))
    lv.name -> lv.irType

  private def visitParams(x: syntax.ListParams, arg: A): Map[String, ir.IRType] = {
    // NOTE: uses ListMap instead of SortedMap, because the orders are presumed to
    // match between calls and param lists within the same file.
    x.asScala.toSeq.map(_.accept(this, arg)).to(ListMap)
  }

  override def visit(x: syntax.ProcedureSig, arg: A) = {
    val name = unsigilProc(x.procident_)
    val inparams = visitParams(x.listparams_1, arg)
    val outparams = visitParams(x.listparams_2, arg)
    val proc = ir.dsl.EventuallyProcedure(name, inparams, outparams, Nil)
    Declarations.empty.copy(procedures = Map(name -> proc))
  }

  override def visit(x: syntax.Procedure, arg: A) = x.procsig_.accept(this, arg)
}
