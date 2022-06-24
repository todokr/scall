package scall

class SheetState (val height: Int, val width: Int) extends Evaluator with Arithmetic {
  import swing._
  import Formula._

  val cells = Array.ofDim[Cell](height, width)

  case class Cell(row: Int, column: Int) extends Publisher {
    private var v: Double = 0
    def value: Double = v
    def value_=(w: Double): Unit = {
      if(!((v == w) || v.isNaN && w.isNaN)) {
        v = w
        publish(ValueChanged(this))
      }
    }

    private var f: Formula = Empty
    def formula: Formula = f
    def formula_=(f: Formula): Unit = {
      for (c <- references(formula)) deafTo(c)
      this.f = f
      for (c <- references(formula)) listenTo(c)
      value = evaluate(f)
    }

    reactions += {
      case ValueChanged(_) => value = evaluate(formula)
    }

    // テキストはそのまま、それ以外は数式を評価した結果
    override def toString: String = formula match {
      case Textual(s) => s
      case _ => value.toString
    }
  }

  case class ValueChanged(cell: Cell) extends event.Event

  for {
    i <- 0 until height
    j <- 0 until width
  } cells(i)(j) = Cell(i, j)
}