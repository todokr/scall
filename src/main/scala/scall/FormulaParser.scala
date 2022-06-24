package scall

import scala.util.parsing.combinator.RegexParsers

object FormulaParser extends RegexParsers {
  import Formula._

  def parse(input: String): Formula = parseAll(formula, input) match {
    case Success(res, _) => res
    case f: NoSuccess => Textual(s"[${f.msg}]")
  }

  private def ident: Parser[String] = """[a-zA-Z_]\w*""".r
  private def decimal: Parser[String] = """-?\d+(\.\d*)?""".r

  private def cell: Parser[Coord] = """[A-Za-z]+""".r ~ """\d+""".r ^^ { case col ~ row =>
    val column = ColLabelParser.parse(col)
    Coord(row.toInt, column)
  }
  private def range: Parser[Range] = cell ~ ":" ~ cell ^^ { case c1 ~ ":" ~ c2 => Range(c1, c2)}
  private def number: Parser[Number] = decimal ^^ (d => Number(d.toDouble))
  private def textual: Parser[Textual] = """[^=].*""".r ^^ Textual
  private def application: Parser[Application] = ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case f ~ "(" ~ args ~ ")" => Application(f, args)
  }
  private def expr: Parser[Formula] = range | cell | number | application
  private def formula: Parser[Formula] = number | textual | ("=" ~> expr)

  object ColLabelParser {

    /** カラムのラベルをインデックス番号としてパースする
     * eg. AA => 26
     */
    def parse(col: String): Int =
      col.toCharArray.map(_ - 'A')
        .reverse
        .zip(Iterator.from(0))
        .foldLeft(0) { case (acc, (d, beam)) => beam * AlfSize + d + acc }

    private val AlfSize = ('A' to 'Z').size
  }
}
