package scall

import scala.collection.mutable

trait Evaluator { this: SheetState =>
  import Formula._

  type Op = Seq[Double] => Double
  val operations = new mutable.HashMap[String, Op]()

  /** セルの数式を値として評価する */
  def evaluate(f: Formula): Double = try {
    f match {
      case Coord(row, column) => cells(row)(column).value
      case Number(v) => v
      case Textual(_) => 0
      case Application(function, arguments) =>
        val args = arguments.flatMap(evalList)
        operations(function)(args)
    }
  } catch {
    case _: Exception => Double.NaN
  }

  /** 数式が参照するセル */
  def references(f: Formula): Seq[Cell] = f match {
    case Coord(row, column) => Seq(cells(row)(column))
    case Range(Coord(r1, c1), Coord(r2, c2)) =>
      for {
        row    <- r1 to r2
        column <- c1 to c2
      } yield cells(row)(column)
    case Application(_, arguments) => arguments.flatMap(references)
    case _ => Seq.empty
  }

  private def evalList(f: Formula): Seq[Double] = f match {
    case _: Range => references(f).map(_.value)
    case _ => Seq(evaluate(f))
  }
}

/** シートで実行可能な関数 */
trait Arithmetic { this: Evaluator =>
  operations ++= Seq(
    "add"  -> { case Seq(x, y) => x + y },
    "sub"  -> { case Seq(x, y) => x - y },
    "div"  -> { case Seq(x, y) => x / y },
    "mul"  -> { case Seq(x, y) => x * y },
    "mod"  -> { case Seq(x, y) => x % y },
    "sum"  -> { xs => xs.foldLeft(0D)(_ + _) },
    "prod" -> { xs => xs.foldLeft(0D)(_ * _) },
  )
}