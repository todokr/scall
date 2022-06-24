package scall

/** セルに入力された数式 */
sealed trait Formula

object Formula {

  /** セル座標
   * ex. `A3`
   */
  case class Coord(row: Int, column: Int) extends Formula {
    override def toString: String = ('A' + column).toChar.toString + row
  }

  /** セル範囲
   * ex. `A3:B17`
   */
  case class Range(c1: Coord, c2: Coord) extends Formula {
    override def toString: String = s"$c1:$c2"
  }

  /** 浮動小数点
   * ex. `3.1415`
   */
  case class Number(value: Double) extends Formula {
    override def toString: String = value.toString
  }

  /** テキスト
   *  ex. `Department`
   */
  case class Textual(value: String) extends Formula {
    override def toString: String = value
  }

  /** 関数適用。
   * ex. `sum(A1, A2)`
   */
  case class Application(function: String, arguments: Seq[Formula]) extends Formula {
    override def toString: String = s"$function${arguments.mkString("(", ",", ")")}"
  }

  object Empty extends Textual("")
}


