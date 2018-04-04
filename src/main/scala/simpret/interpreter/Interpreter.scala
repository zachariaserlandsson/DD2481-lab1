package simpret.interpreter

import simpret.parser._
import simpret.errors._


final case class EvaluationException(private val message: String, private val x: AST,
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

object Interpreter {
  def errFun(msg: String, x: AST) = throw new EvaluationException(msg, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST) = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case _ => false
  }

  /* evaluation function for taking one step at a time */
  def step(x: AST, store: Map[String, AST]): Option[(AST, Map[String, AST])] = {
    x match {
      case Variable(id) => {
        if(store.contains(id)) {
          Option((store(id), store))
        } else {
          errFun("Variable not found", x)
        }
      }
      case CondExp(BoolLit(b), e1, e2) if b => Option(e1, store)
      case CondExp(BoolLit(b), e1, e2) if !b => Option(e2, store)
      case CondExp(IntLit(i), e1, e2) => errFun("condition should be a boolean expression", x)
      case CondExp(c, e1, e2) => {
        val (cp, storep) = step(c, store).get
        Option((CondExp(cp, e1, e2), storep))
      }
      case IsZeroExp(IntLit(i)) if i == 0 => Option(BoolLit(true), store)
      case IsZeroExp(IntLit(i)) if i != 0 => Option(BoolLit(false), store)
      case IsZeroExp(BoolLit(b)) => errFun("iszero expects integer value", x)
      case IsZeroExp(e) => {
        val (ep, storep) = step(e, store).get
        Option((IsZeroExp(ep), storep))
      }
      case PlusExp(BoolLit(b), _) => errFun("addition operator should be integer expression", x)
      case PlusExp(_, BoolLit(b)) => errFun("addition operator should be integer expression", x)
      case PlusExp(IntLit(i1), IntLit(i2)) => Option(IntLit(i1 + i2), store)
      case PlusExp(IntLit(i1), e) => {
        val (ep, storep) = step(e, store).get
        Option((PlusExp(IntLit(i1), ep), storep))
      }
      case PlusExp(e1, e2) => {
        val (e1p, storep) = step(e1, store).get
        Option((PlusExp(e1p, e2), storep))
      }
      case AssignExp(id, e) if isvalue(e) => Option(e, store + (id -> e))
      case AssignExp(id, e) => {
        val (ep, storep) = step(e, store).get
        Option((AssignExp(id, ep), storep))
      }
      case SeqExp(e1, e2) if isvalue(e1) => Option(e2, store)
      case SeqExp(e1, e2) => {
        val (e1p, storep) = step(e1, store).get
        Option((SeqExp(e1p, e2), storep))
      }
      case _ if isvalue(x) => None
      case _ => errFun("Invalid syntax tree", x)
    }
  }

  /* evaluation function to iterate the steps of evaluation */
  def eval(x: AST, store: Map[String, AST] = Map.empty): AST = {
    step(x, store) match {
      case None => x
      case Some ((x1, store1)) => eval(x1, store1)
    }
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[EvaluationError, AST] = {
    try {
      Right(eval(x))
    } catch {
      case EvaluationException (msg, xe, _) =>
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(xe)
        Left(EvaluationError(Location.fromPos(xe.pos), msg2))
    }
  }
}
