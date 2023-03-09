package cs320

import Value._

object Implementation extends Template {

  def numVop(op: (BigInt, BigInt) => BigInt, excludeZero: Boolean): (Value, Value) => IntV = (_,_) match {
    case (IntV(x), IntV(y)) => if (excludeZero && y == 0) error(s"second argument is zero") else IntV(op(x,y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }

  def numVCompOp(op: (BigInt, BigInt) => Boolean): (Value, Value) => BooleanV = (_,_) match {
    case (IntV(x), IntV(y)) => BooleanV(op(x,y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }

  val intVAdd = numVop(_ + _, false)
  val intVMul = numVop(_ * _, false)
  val intVDiv = numVop(_ / _, true)
  val intVMod = numVop(_ % _, true)
  val intVEq = numVCompOp(_ == _)
  val intVLt = numVCompOp(_ < _)

  type Env = Map[String, Value]

  def interp(expr: Expr, env: Env): Value = expr match {
    case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
    case IntE(v) => IntV(v)
    case BooleanE(v) => BooleanV(v)
    case Add(l, r) => intVAdd(interp(l, env), interp(r, env))
    case Mul(l, r) => intVMul(interp(l, env), interp(r, env))
    case Div(l, r) => intVDiv(interp(l, env), interp(r, env))
    case Mod(l, r) => intVMod(interp(l, env), interp(r, env))
    case Eq(l, r) => intVEq(interp(l, env), interp(r, env))
    case Lt(l, r) => intVLt(interp(l, env), interp(r, env))
    case If(c, t, f) => interp(c, env) match {
      case BooleanV(true) => interp(t, env)
      case BooleanV(false) => interp(f, env)
      case v => error(s"not a condition: $v")
    }
    case TupleE(exprs) => TupleV(exprs.map(interp(_, env)))
    case Proj(e, i) => interp(e, env) match {
      case TupleV(vs) => if (vs.length >= i) vs(i-1) else error(s"index out of range: $i")
      case v => error(s"not a tuple: $v")
    }
    case NilE => NilV
    case ConsE(h, t) => 
      val head = interp(h, env)
      val tail = interp(t, env)
      tail match {
        case ConsV(_, _) | NilV => ConsV(head, tail)
        case v => error(s"not a list: $v")
      }
    case Empty(e) => interp(e, env) match {
      case NilV => BooleanV(true)
      case ConsV(_, _) => BooleanV(false)
      case v => error(s"not a list: $v")
    }
    case Head(e) => interp(e, env) match {
      case ConsV(h, _) => h
      case v => error(s"not non-empty list: $v")
    }
    case Tail(e) => interp(e, env) match {
      case ConsV(_, t) => t
      case v => error(s"not non-empty list: $v")
    }
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Fun(params, body) => CloV(params, body, env)
    case RecFuns(funcs, body) => 
      val closures = funcs.map(f => CloV(f.parameters, f.body, env))
      val mappings = closures.zipWithIndex.map {case (clov, idx) => funcs(idx).name -> clov}
      val nenv = env ++ mappings.toMap
      closures.foreach {case clov => clov.env = nenv}
      interp(body, nenv)
    case App(f, args) => interp(f, env) match {
      case CloV(params, b, fenv) => if (args.length != params.length) error(s"wrong arity") else interp(b, fenv ++ (params zip args.map(interp(_, env))).toMap)
      case _ => error(s"not a closure")
    }
    case Test(e, typ) => (interp(e, env), typ) match {
      case (IntV(_), IntT) | (BooleanV(_), BooleanT) | (TupleV(_), TupleT) | (NilV, ListT) | (ConsV(_,_), ListT) | (CloV(_,_,_), FunctionT) => BooleanV(true)
      case (_, _) => BooleanV(false)
    }
  }

  def interp(expr: Expr): Value = interp(expr, Map())

}
