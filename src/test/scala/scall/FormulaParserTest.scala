package scall

import org.scalatest.funsuite.AnyFunSuite

class FormulaParserTest extends AnyFunSuite {
  import FormulaParser.ColLabelParser

  test("ColLabelParser parses A as 0") {
    val actual = ColLabelParser.parse("A")
    assert(actual == 0)
  }

  test("ColLabelParser parses AA as 26") {
    val targets = Seq("AA", "AB", "AC", "AD", "AE")
    val actual = targets.map(ColLabelParser.parse)
    assert(actual == Seq(26, 27, 28, 29, 30))
  }
}
