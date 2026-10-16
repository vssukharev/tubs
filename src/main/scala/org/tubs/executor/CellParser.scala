
package org.tubs.executor

import scala.util.parsing.combinator.RegexParsers
import org.tubs.utils.{Todo, Unreachable}
import org.tubs.table.CellKey
import org.tubs.view

enum BinaryOp:
  case Range, Plus, Minus, Prod, Div, Mod,
       Power, Lt, Gt, Le, Ge, Eq, Neq, And, Or     

enum UnaryOp:
  case Plus, Minus, Not

enum CellCoord:
  case Static(index: Int)
  case Dynamic(expr: CellAST)

enum CellAST:
  case LitString(str: String)
  case LitInteger(v: Int)
  case LitFloating(v: Double)
  case LitBoolean(v: Boolean)
  case Variable(id: String)
  case Binary(op: BinaryOp, a: CellAST, b: CellAST)
  case Unary(op: UnaryOp, a: CellAST)
  case CellRef(col: CellCoord, row: CellCoord)
  case IfExpr(cond: CellAST, thenBranch: CellAST, elseBranch: CellAST)
  case ForExpr(it: String, acc: String, init: CellAST, range: CellAST, body: CellAST)
  case SyntaxError

object CellAST:
  /**
   * Builds an AST for formula. First, it checks, whether
   * the formula starts with "=" which indicates arithmetic
   * clause, otherwise the whole AST will be a single string
   * literal node, containing the formal itself.
   *
   * In case of an error, sends a message to user and make
   * a node SyntaxError.
   */
  def apply(key: CellKey, formula: String): CellAST =
    import FormulaParser.*
    val trimmed = formula.trim
    if !trimmed.startsWith("=") then 
      CellAST.LitString(formula)
    else
      val code = trimmed.drop(1)
      parseAll(expr, code) match
        case Success(result, _) => result
        case failure: NoSuccess =>
          view.error(s"Syntax error in $key: ${failure.msg}")
          CellAST.SyntaxError


object FormulaParser extends RegexParsers:
  private val keywords = Set("if", "then", "else", "for", "in", "do", "mod", "and", "or", "not", "true", "false")

  /**
   * Converts column letters to the corresponding index in Table
   */
  def colLettersToIndex(letters: String): Int =
    letters.foldLeft(0)((acc, c) => acc * 26 + (c - 'A' + 1))

  // --- Top-level Expression ---
  def expr: Parser[CellAST] = forExpr | ifExpr | rangeExpr 
  
  // --- for <it>, <acc>=<init> in <range> do <body> ---
  def forExpr: Parser[CellAST] =
    ("for" ~> variableDecl <~ ",") ~ (variableDecl <~ "=") ~ expr ~ ("in" ~> expr) ~ ("do" ~> expr) ^^ {
      case it ~ acc ~ init ~ range ~ body => CellAST.ForExpr(it, acc, init, range, body)
    }
  
  // --- if <cond> then <thenBranch> else <elseBranch>
  def ifExpr: Parser[CellAST] =
    ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
      case cond ~ thenBranch ~ elseBranch => CellAST.IfExpr(cond, thenBranch, elseBranch)
    }

  // --- Range (..) ---
  def rangeExpr: Parser[CellAST] =
    orExpr ~ (".." ~> orExpr).? ^^ {
      case left ~ Some(right) => CellAST.Binary(BinaryOp.Range, left, right)
      case left ~ None        => left
    }

  // --- Logical OR ---
  def orExpr: Parser[CellAST] =
    andExpr ~ ("or" ~> andExpr).* ^^ { 
      case head ~ rest => rest.foldLeft(head)(CellAST.Binary(BinaryOp.Or, _, _))
    }
    
  // --- Logical AND ---
  def andExpr: Parser[CellAST] =
    relExpr ~ ("and" ~> relExpr).* ^^ { 
      case head ~ rest => rest.foldLeft(head)(CellAST.Binary(BinaryOp.And, _, _))
    }

  // --- Relational Comparisons ---
  def relExpr: Parser[CellAST] =
    addExpr ~ (relOp ~ addExpr).? ^^ {
      case left ~ Some(op ~ right) => CellAST.Binary(op, left, right)
      case left ~ None             => left
    }
    
  def relOp: Parser[BinaryOp] =
    "<=" ^^^ BinaryOp.Le | ">=" ^^^ BinaryOp.Ge | "==" ^^^ BinaryOp.Eq |
    "!=" ^^^ BinaryOp.Neq | "<" ^^^ BinaryOp.Lt | ">" ^^^ BinaryOp.Gt

  // --- Additive ( + , - ) ---
  def addExpr: Parser[CellAST] =
    multExpr ~ (("+" | "-") ~ multExpr).* ^^ {
      case head ~ rest => rest.foldLeft(head) {
        case (acc, "+" ~ next) => CellAST.Binary(BinaryOp.Plus, acc, next)
        case (acc, "-" ~ next) => CellAST.Binary(BinaryOp.Minus, acc, next)
        case (_, _ ~ _)        => Unreachable("FormulaParser.addExpr")
      }
    }

  // --- Multiplicative ( * , / , mod ) ---
  def multExpr: Parser[CellAST] =
    powerExpr ~ (("*" | "/" | "mod") ~ powerExpr).* ^^ {
      case head ~ rest => rest.foldLeft(head) {
        case (acc, "*" ~ next) => CellAST.Binary(BinaryOp.Prod, acc, next)
        case (acc, "/" ~ next) => CellAST.Binary(BinaryOp.Div, acc, next)
        case (acc, "mod" ~ next) => CellAST.Binary(BinaryOp.Mod, acc, next)
        case (_, _ ~ _)        => Unreachable("FormulaParser.multExpr")
      }
    }
  
  // --- Power ( ^ ) - right associative ---
  def powerExpr: Parser[CellAST] =
    unaryExpr ~ ("^" ~> powerExpr).? ^^ {
      case base ~ Some(exp) => CellAST.Binary(BinaryOp.Power, base, exp)
      case base ~ None      => base
    }

  // --- Unary ( + , - , not ) ---
  def unaryExpr: Parser[CellAST] =
    ("+" ~> unaryExpr) ^^ (CellAST.Unary(UnaryOp.Plus, _))  |
    ("-" ~> unaryExpr) ^^ (CellAST.Unary(UnaryOp.Minus, _)) |
    ("not" ~> unaryExpr) ^^ (CellAST.Unary(UnaryOp.Not, _)) |
    primary

  // --- Primary expressions ---
  def primary: Parser[CellAST] =
    litBoolean | litFloating | litInteger | cellRef | variable | "(" ~> expr <~ ")"

  def litBoolean: Parser[CellAST] =
    "true" ^^^ CellAST.LitBoolean(true) | "false" ^^^ CellAST.LitBoolean(false)

  def litFloating: Parser[CellAST] =
    """[0-9]+\.[0-9]+""".r ^^ ((s: String) => CellAST.LitFloating(s.toDouble))

  def litInteger: Parser[CellAST] =
    """[0-9]+""".r ^^ ((s: String) => CellAST.LitInteger(s.toInt))

  // Check a variable for not being a keyword
  def variable: Parser[CellAST] =
    variableDecl ^^ (CellAST.Variable(_))

  def variableDecl: Parser[String] =
    ident ^? ({
      case (id: String) if !keywords.contains(id) => id
    }, (id: String) => s"'$id' is a reserved keyword")

  // --- Cell Reference ---
  def cellRef: Parser[CellAST] =
    colLetters ~ """[1-9][0-9]*""".r ^^ { case colStr ~ rowStr =>
      CellAST.CellRef(CellCoord.Static(colLettersToIndex(colStr)), CellCoord.Static(rowStr.toInt))
    } |
    colLetters ~ ("[" ~> expr <~ "]") ^^ { case colStr ~ rowExpr =>
      CellAST.CellRef(CellCoord.Static(colLettersToIndex(colStr)), CellCoord.Dynamic(rowExpr))
    } |
    ("[" ~> expr <~ "]") ~ """[1-9][0-9]*""".r ^^ { case colExpr ~ rowStr =>
      CellAST.CellRef(CellCoord.Dynamic(colExpr), CellCoord.Static(rowStr.toInt))
    } |
    ("[" ~> expr <~ "]") ~ ("[" ~> expr <~ "]") ^^ { case colExpr ~ rowExpr =>
      CellAST.CellRef(CellCoord.Dynamic(colExpr), CellCoord.Dynamic(rowExpr))
    }

  def colLetters: Parser[String] = """[A-Z]+""".r
  def ident: Parser[String] = """[a-z][a-zA-Z0-9]*""".r


