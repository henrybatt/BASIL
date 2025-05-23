package analysis.data_structure_analysis

import analysis.data_structure_analysis.OSet.{Top, Values}
import ir.eval.BitVectorEval.bv2SignedInt
import ir.*
import ir.transforms.{AbstractDomain, worklistSolver}
import util.SVALogger as Logger

import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

def mapMerge[K, V](a: Map[K, V], b: Map[K, V], f: (V, V) => V): Map[K, V] = {
  var merged = a
  for ((key, value) <- b)
    merged = merged.updatedWith(key) {
      case None => Some(value)
      case Some(existing) => Some(f(existing, value))
    }
  merged
}

def getSymbolicValues[T <: Offsets](p: Procedure)(using valSetDomain: SymValSetDomain[T]): SymValues[T] = {
  Logger.info(s"Generating Symbolic Values for ${p.name}")
  val symValuesDomain = SymValuesDomain()
  val symValSolver = worklistSolver[SymValues[T], symValuesDomain.type](symValuesDomain)
  symValSolver
    .solveProc(p)
    ._2
    .values
    .fold(symValuesDomain.bot)((x, y) => symValuesDomain.join(x, y, p.entryBlock.get))
}

def isPlaceHolder(base: SymBase): Boolean = {
  base match
    case known: Known => false
    case unknown: Unknown => true
}

// a symbolic base address
sealed trait SymBase

// Known heap, malloc, global address
sealed trait Known extends SymBase

case class Heap(call: DirectCall) extends Known {
  assert(
    call.target.name.startsWith("malloc") || call.target.name.startsWith("calloc"),
    "Expected a call to malloc-like function"
  )
  private val procName = call.parent.parent.name
  private val label = labelToPC(call.label)
  override def toString: String = s"Heap(${procName}_$label)"
}

case class Stack(proc: Procedure) extends Known {
  override def toString: String = s"Stack(${proc.name})"
}

sealed trait Const extends Known
case object Global extends Const
case object NonPointer extends Const

// placeholder sym bases for loaded and in/out params
sealed trait Unknown extends SymBase
case class Par(proc: Procedure, param: LocalVar) extends Unknown {
  private val procName = proc.name
  override def toString: String = s"Par(${procName}_${param.name})"
}

case class Ret(call: DirectCall, param: LocalVar) extends Unknown {
  private val procName = call.parent.parent.name
  private val targetName = call.target.name
  private val label = labelToPC(call.label)
  override def toString: String = s"Ret(${procName}_${targetName}_${param.name}_$label)"
}

case class Loaded(load: MemoryLoad) extends Unknown {
  private val procName = load.parent.parent.name
  private val label = labelToPC(load.label)
  override def toString: String = s"Loaded(${procName}_$label)"
}

def labelToPC(label: Option[String]): String = {
  //  assert(label.nonEmpty)
  label.getOrElse("no_label").takeWhile(_ != ':')
}

def DSAVartoLast(DSAVar: LocalVar): LocalVar = {
  if DSAVar.index > 1 then LocalVar(DSAVar.varName, DSAVar.irType, DSAVar.index - 1)
  else if DSAVar.index == 1 then LocalVar(DSAVar.varName + "_in", DSAVar.irType)
  else throw Exception(s"Attempted to get previous version of DSA variable without one")
}

object DSAVarOrdering extends Ordering[LocalVar] {
  def compare(a: LocalVar, b: LocalVar) = {
    if a.name == b.name then 0
    else if a.name.endsWith("_in") || b.name.endsWith("_out") then -1
    else if a.name.endsWith("_out") then 1
    else a.index - b.index
  }
}

def toOffsetMove[T <: Offsets](op: BinOp, arg: BitVecLiteral | T, domain: OffsetDomain[T]): T => T = {
  val value = arg match {
    case bv: BitVecLiteral => domain.init(bv2SignedInt(bv).toInt)
    case off: T => off
  }
  op match
    case BVADD => (i: T) => domain.add(i, value)
    case BVSUB => (i: T) => domain.add(i, value, neg = true)
    case _ => throw Exception(s"Usupported Binary Op $op")
}

trait Offsets {
  def toOffsets: Set[Int]
  def toIntervals: Set[DSInterval]
}
trait OffsetDomain[T <: Offsets] extends AbstractDomain[T] {
  def init(i: Int): T
  def init(s: Set[Int]): T
  def shouldWiden(v: T): Boolean
  def transform(v: T, f: Int => Int): T
  def transform(v: T, f: T => T)(implicit dummyImplicit: DummyImplicit): T = f(v)
  def add(a: T, b: T, neg: Boolean = false): T
}

enum OSet extends Offsets {
  case Top
  case Values(v: Set[Int])

  override def toIntervals: Set[DSInterval] = {
    this match
      case OSet.Top => Set(DSInterval.Top)
      case OSet.Values(v) => v.map(i => DSInterval(i, i + 1))
  }
  override def toOffsets: Set[Int] = {
    this match
      case OSet.Top => throw Exception("Attempted to retrieve offsets from Top")
      case OSet.Values(v) => v
  }
}

given IntervalDomain: OffsetDomain[DSInterval] with {

  override def init(i: Int): DSInterval = DSInterval(i, i)

  override def init(s: Set[Int]): DSInterval = DSInterval(s.min, s.max)

  override def shouldWiden(v: DSInterval): Boolean = false

  override def transform(v: DSInterval, f: Int => Int): DSInterval = {
    v.move(f)
  }

  override def widen(a: DSInterval, b: DSInterval, pos: Block): DSInterval = DSInterval.Top

  override def join(a: DSInterval, b: DSInterval, pos: Block): DSInterval = a.join(b)

  override def transfer(a: DSInterval, b: Command): DSInterval = ???

  override def top: DSInterval = DSInterval.Top

  override def bot: DSInterval = DSInterval.Bot

  override def add(a: DSInterval, b: DSInterval, neg: Boolean): DSInterval = {
    (a, b) match {
      case (DSInterval.Top, _) => DSInterval.Top
      case (_, DSInterval.Top) => DSInterval.Top
      case (a, DSInterval.Bot) => a
      case (DSInterval.Bot, b) => b
      case (DSInterval.Value(s1, e1), DSInterval.Value(s2, e2)) =>
        val (s3, e3) = if neg then (-s2, -e2) else (s2, e2)
        val o1 = s1 + s3
        val o2 = s1 + e3
        val o3 = s3 + e1
        val o4 = e3 + e1

        val values = Set(o1, o2, o3, o4)
        DSInterval(values.reduce(Math.min), values.reduce(Math.max))
    }
  }
}

given OSetDomain: OffsetDomain[OSet] with {
  override def join(a: OSet, b: OSet, pos: Block): OSet = {
    (a, b) match
      case (OSet.Top, _) => OSet.Top
      case (_, OSet.Top) => OSet.Top
      case (OSet.Values(x), OSet.Values(y)) => OSet.Values(x.union(y))
  }
  override def transfer(a: OSet, b: Command): OSet = ???
  override def top: OSet = OSet.Top
  override def bot: OSet = OSet.Values(Set.empty)
  override def widen(a: OSet, b: OSet, pos: Block): OSet = {
    OSet.Top
  }
  override def init(i: Int): OSet = Values(Set(i))

  override def shouldWiden(v: OSet): Boolean = {
    v match
      case OSet.Top => false
      case OSet.Values(v) => v.size > 10
  }

  override def transform(v: OSet, f: Int => Int): OSet = {
    v match
      case OSet.Top => OSet.Top
      case OSet.Values(v) => Values(v.map(f))
  }

  override def init(s: Set[Int]): OSet = Values(s)

  override def add(a: OSet, b: OSet, neg: Boolean): OSet = {
    if a == Top || b == OSet.Top then OSet.Top
    else if neg then init(a.toOffsets.flatMap(i => b.toOffsets.map(j => i - j)))
    else init(a.toOffsets.flatMap(i => b.toOffsets.map(j => i + j)))
  }
}

case class SymValSet[T <: Offsets](state: Map[SymBase, T])

object SymValSet {
  def transform[T <: Offsets](s: SymValSet[T], f: Int => Int)(using oDomain: OffsetDomain[T]): SymValSet[T] = {
    SymValSet(s.state.map((base, offsets) => (base, oDomain.transform(offsets, f))))
  }
}

given OSetSymValSetDomain: SymValSetDomain[OSet] = SymValSetDomain[OSet]()
given IntervalSymValDomain: SymValSetDomain[DSInterval] = SymValSetDomain[DSInterval]()
class SymValSetDomain[T <: Offsets](using val offsetDomain: OffsetDomain[T]) extends AbstractDomain[SymValSet[T]] {

  override def join(a: SymValSet[T], b: SymValSet[T], pos: Block = Block("")): SymValSet[T] = {
    SymValSet(mapMerge(a.state, b.state, (x, y) => offsetDomain.join(x, y, pos)))
  }

  override def widen(a: SymValSet[T], b: SymValSet[T], pos: Block): SymValSet[T] = {
    val update = a.state
      .collect {
        case (base: SymBase, offsets: T) if b.state.contains(base) && b.state(base) != offsets =>
          (base, offsetDomain.widen(offsets, b.state(base), pos))
        case (base: SymBase, offsets: T) if offsetDomain.shouldWiden(offsets) =>
          (base, offsetDomain.widen(offsets, b.state(base), pos))
      }
    join(a, join(b, SymValSet(update), pos), pos)
  }

  def init(base: SymBase, value: Int = 0): SymValSet[T] = {
    SymValSet(Map(base -> offsetDomain.init(value)))
  }

  def init(base: SymBase, values: Set[Int]): SymValSet[T] = {
    SymValSet(Map(base -> offsetDomain.init(values)))
  }
  def init(base: SymBase, offsets: T): SymValSet[T] = {
    SymValSet(Map(base -> offsets))
  }

  override def transfer(a: SymValSet[T], b: Command): SymValSet[T] = ???
  def transform(s: SymValSet[T], f: Int => Int): SymValSet[T] = {
    SymValSet(s.state.map((base, offsets) => (base, offsetDomain.transform(offsets, f))))
  }

  def transform(s: SymValSet[T], f: T => T)(implicit dummyImplicit: DummyImplicit): SymValSet[T] = {
    SymValSet(s.state.view.mapValues(f).toMap)
  }
  override def top: SymValSet[T] = ???
  override def bot: SymValSet[T] = SymValSet(Map.empty)
}

case class SymValues[T <: Offsets](state: Map[LocalVar, SymValSet[T]]) {
  def apply(lvar: LocalVar): SymValSet[T] = state(lvar)
}

object SymValues {

  private def sort[T <: Offsets](state: Map[LocalVar, SymValSet[T]]): SortedMap[LocalVar, SymValSet[T]] = {
    require(state.keys.map(f => f.name.take(3)).toSet.size <= 1, "Expected one or less Variable in state")
    SortedMap.sortedMapFactory[LocalVar, SymValSet[T]](DSAVarOrdering).fromSpecific(state)
  }

  // get the Symbolic Values of local Vars with prefix in their names
  private def get[T <: Offsets](state: Map[LocalVar, SymValSet[T]], prefix: String): SymValues[T] = {
    val newState = state.collect {
      case entry @ (loc: LocalVar, _) if loc.name.startsWith(prefix) => entry
    }
    SymValues(newState)
  }

  def getSorted[T <: Offsets](s: SymValues[T], prefix: String): SortedMap[LocalVar, SymValSet[T]] = {
    sort(get(s.state, prefix).state)
  }

  def pretty[T <: Offsets](s: SymValues[T]): String = {
    s.state.map((lvar, valSet) => s"$lvar: $valSet").mkString("\n")
  }

  @tailrec
  final def exprToSymValSet[T <: Offsets](symValues: SymValues[T])(using
    symValSetDomain: SymValSetDomain[T]
  )(
    expr: Expr,
    transform: (T => T) = identity[T],
    replace: LocalVar => LocalVar = identity,
    block: Block = Block("")
  ): SymValSet[T] = {
    val oDomain = symValSetDomain.offsetDomain
    expr match
      case literal @ BitVecLiteral(value, size) =>
        symValSetDomain.init(Global, transform(oDomain.init(bv2SignedInt(literal).toInt)))
      case literal @ IntLiteral(value) => symValSetDomain.init(Global, value.toInt)
      case Extract(end, start, body) if end - start >= 64 => exprToSymValSet(symValues)(body, transform)
      case Extract(32, 0, body) =>
        exprToSymValSet(symValues)(body, transform) // todo incorrectly assuming value is preserved
      case ZeroExtend(extension, body) => exprToSymValSet(symValues)(body, transform)
      case binExp @ BinaryExpr(BVADD | BVSUB, arg1, arg2: BitVecLiteral) =>
        val oPlus = toOffsetMove(binExp.op, arg2, oDomain)
        exprToSymValSet(symValues)(arg1, oPlus)
      case binExp @ BinaryExpr(BVADD | BVSUB, arg1, arg2: Expr)
          if arg2.variables.size == 1 && symValues.state
            .getOrElse(arg2.variables.head.asInstanceOf[LocalVar], symValSetDomain.bot)
            .state
            .keySet == Set(Global) =>
        val oPlus =
          toOffsetMove(binExp.op, symValues.state(arg2.variables.head.asInstanceOf[LocalVar]).state(Global), oDomain)
        exprToSymValSet(symValues)(arg1, oPlus)
      case variable: LocalVar =>
        symValSetDomain.transform(symValues.state.getOrElse(replace(variable), symValSetDomain.bot), transform)
      case Extract(end, start, body) if end - start < 64 =>
        symValSetDomain.init(NonPointer, oDomain.top)
      case BinaryExpr(BVCOMP, _, _) => symValSetDomain.transform(symValSetDomain.init(NonPointer, Set(0, 1)), transform)
      case e @ (BinaryExpr(_, _, _) | SignExtend(_, _) | UnaryExpr(_, _)) =>
        val updated = e.variables
          .map(_.asInstanceOf[LocalVar])
          .collect { case locVar: LocalVar if symValues.state.contains(locVar) => symValues.state(locVar) }
          .flatMap(_.state)
          .map((base, _) => (base, oDomain.top))
          .toMap
        SymValSet(updated)
      case _ => ???
  }
}

class SymValuesDomain[T <: Offsets](using symValSetDomain: SymValSetDomain[T]) extends AbstractDomain[SymValues[T]] {

  private val stackPointer = LocalVar("R31_in", BitVecType(64))
  private val linkRegister = LocalVar("R30_in", BitVecType(64)) // Register("R30", 64)
  private val framePointer = LocalVar("R29_in", BitVecType(64))
  private val calleePreserved = (19 to 29).appended(31).map(i => "R" + i)
  private val implicitFormals = Set(stackPointer, linkRegister, framePointer)

  private val count: mutable.Map[Block, Int] = mutable.Map.empty.withDefault(_ => 0)

  override def widen(a: SymValues[T], b: SymValues[T], pos: Block): SymValues[T] = {
    val update = a.state.collect {
      case (localVar: LocalVar, symValSet: SymValSet[T])
          if b.state.contains(localVar) && symValSet != b.state(localVar) =>
        (localVar, symValSetDomain.widen(symValSet, b.state(localVar), pos))
    }

    joinHelper(a, joinHelper(b, SymValues(update), pos), pos)
  }

  private def joinHelper(a: SymValues[T], b: SymValues[T], pos: Block): SymValues[T] = {
    SymValues(mapMerge(a.state, b.state, (x, y) => symValSetDomain.join(x, y, pos)))
  }

  private def procInitState(b: Block): Map[LocalVar, SymValSet[T]] = {
    val proc = b.parent
    Map(
      stackPointer -> symValSetDomain.init(Stack(proc)),
      linkRegister -> symValSetDomain.init(Par(proc, linkRegister)),
      framePointer -> symValSetDomain.init(Par(proc, framePointer))
    ) ++
      proc.formalInParam
        .filterNot(param => implicitFormals.map(f => f.name.take(3)).exists(name => param.name.startsWith(name)))
        .toSet
        .map(param => (param, symValSetDomain.init(Par(proc, param))))
        .toMap
  }

  override def init(b: Block): SymValues[T] = {
    if b.isEntry then SymValues(procInitState(b))
    else bot
  }

  override def join(a: SymValues[T], b: SymValues[T], pos: Block): SymValues[T] = {
    count.update(pos, count(pos) + 1)
    if count(pos) < 1000 then joinHelper(a, b, pos) else widen(a, b, pos)
  }

  override def transfer(a: SymValues[T], b: Command): SymValues[T] = {
    val proc = b.parent.parent
    val block = b.parent
    b match
      case LocalAssign(lhs: LocalVar, rhs: Expr, _) =>
        val update = SymValues(Map(lhs -> SymValues.exprToSymValSet(a)(rhs)))
        join(a, update, block)
      case load @ MemoryLoad(lhs: LocalVar, _, rhs, _, size, label) =>
        val update = SymValues(Map(lhs -> symValSetDomain.init(Loaded(load))))
        join(a, update, block)
      case mal @ DirectCall(target, outParams, inParams, label)
          if target.name.startsWith("malloc") || target.name.startsWith("calloc") =>
        val (malloc, others) = outParams.values
          .map(_.asInstanceOf[LocalVar])
          .partition(outParam => outParam.name.startsWith("R0")) // malloc ret R0
        assert(malloc.size == 1, s"SVA expected only one R0 returned from $mal")
        val update = SymValues( // assume call to malloc only changes the value of R0
          Map(malloc.head -> symValSetDomain.init(Heap(mal)))
        )
        join(a, update, block)
      case call @ DirectCall(target, outParams, inParams, label) =>
        val unchanged = Set("R29", "R30", "R31")
        val (maintained, notMaintained) =
          outParams.values
            .map(_.asInstanceOf[LocalVar])
            .partition(v => unchanged.exists(r => v.name.startsWith(r)))
        val n = maintained
          .map(l =>
            (
              l,
              inParams.collectFirst {
                case (in, act) if in.name.take(3) == l.name.take(3) => SymValues.exprToSymValSet(a)(act)
              }.get
            )
          )
          .toMap
        val retInitSymValSet = SymValues(
          notMaintained
            .map(param => (param, symValSetDomain.init(Ret(call, param))))
            .toMap
            ++ n
        )
        join(a, retInitSymValSet, block)
      case ind: IndirectCall => a // TODO possibly map every live variable to top
      case ret: Return =>
        val update = SymValues(ret.outParams.map { case (outVar: LocalVar, value: Expr) =>
          outVar -> SymValues.exprToSymValSet(a)(value)
        })

        join(a, update, block)
      case _ => a
  }

  override def top: SymValues[T] = ???

  override def bot: SymValues[T] = SymValues(Map.empty)
}
