package hwk6

import common._
import interprocedural._

import scala.collection.mutable
case class VarMap(vars: Set [(String, String)]) extends Lattice [VarMap]{
  def lub(that: VarMap) = VarMap(vars.union(that.vars))
  val zeros = for ((x,z) <- vars if x == Util.zero) yield z

  override def toString = "{"+vars.toList.sortBy(x=>x).mkString(", ")+"}"
  def gen (y:String) = VarMap(vars + ((Util.zero, y)))
  def gen (ys:List[String]) = VarMap(vars ++ ys.map(y=> (Util.zero,y)))

  def kill_gen(y:String,e : Expression)={
    val fv = Util.fv(e)
    def h (x:String) = if (fv.contains(x)) Set(y) else if (x!=y) Set(x) else Set[String]()
    VarMap(for((x,y)<- vars;  z <- h(y)) yield (x,z))
  }
}


case class IFDS(stmt: Statement) extends Analysis[VarMap] {
  val cfg = ForwardCFG(stmt)
  val entry = real_entry
  val exit = real_exit

  val extremalValue = VarMap(Set((Util.zero, Util.zero)) ++ Util.vars(stmt).map(x=> (Util.zero,x)))
  val bottom = VarMap(Set())

  def transfer(node: Node, l: VarMap) = node match {
    case IntraNode(stmt) => transfer(stmt, l)

    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_,ps,stmt)))) => {
      (VarMap(l.vars.map(x=>(x._2 ,x._2)))).gen((Util.vars(stmt) -- ps.map(p=>p.str) + Util.ret).toList )
    }
    case ExitNode(Some(_)) => VarMap(l.vars.filter( x=> x == Util.ret) )
    //case ExitNode(Some(_)) => VarMap(l.vars.intersect(Set(Util.ret))) // keep the return variable if it is present
    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      def h(args: List[Expression]) = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(p => p.str)
        val ye = params zip args
        val r1 = (Set(Util.zero) ++ params.drop(args.length)).map(x=> (Util.zero,x))

        val r2 = for((y,e) <- ye; (x,z) <- l.vars if Util.fv(e).contains(z)) yield (x,y)
       // val s = for((p, e) <- params zip args; if initialized(e, l)) yield p
       // Vars(params.toSet -- s)     // function f(x, y) { ... }   f(10);
        VarMap(r1++r2)
      }

      stmt match {
        case ExprStmt(FuncCall(_, args)) => h(args)                       // f(a);
        case ExprStmt(AssignExpr(_, _, FuncCall(_, args))) => h(args)     // x = f(a);
        case VarDeclStmt(_, FuncCall(_, args)) => h(args)                 // var x = f(a);
        case _ => bottom
      }
    }

    case n@RetNode(stmt, from) => {
      val call = cfg.call_ret(n)
      val lc1 = entry(call)
      val lc2 = exit(call)


      def h(x: String) = {
        val r = for((y,z) <- lc2.vars; if l.vars.contains((x,z))) yield (y,x)
        VarMap(lc1.vars.filter( y=> y._2 !=x) ++r)
      }

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => h(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => h(x)    // var x = f(e);
        case _ => lc1 // f(e);
      }
    }

    case _ => l
  }



  def transfer(stmt: Statement, l: VarMap) = {
    //def kill_gen(y: String, e: Expression) = Vars(if (initialized(e, l)) l.vars - y else l.vars + y)

    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) => {
        e match {
          case EmptyExpr() => l.gen(y)
          case _ => l.kill_gen(y,e)                 // var y = e;
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => l.kill_gen(y, e) // y = e;
      case ReturnStmt(e) => l.kill_gen(Util.ret, e)  // return e;
      case _ => l
    }
  }
}