package scall

import java.awt
import scala.swing.Table.AutoResizeMode
import scala.swing.event.TableUpdated
import scala.swing._

class SpreadSheet(val height: Int, val width: Int) extends ScrollPane {

  private val state = new SheetState(height, width)
  import state._

  private val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = AutoResizeMode.Off
    showGrid = true
    gridColor = new awt.Color(150, 150, 150)

    override def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
      if (focused) new TextField(userData(row, column))
      else new Label(cells(row)(column).toString) {
        xAlignment = Alignment.Right
      }
    }

    def userData(row: Int, column: Int): String = {
      val v = this.apply(row, column)
      if (v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(_, rows, column) =>
        for (row <- rows) cells(row)(column).formula = FormulaParser.parse(userData(row, column))
      case ValueChanged(cell) => updateCell(cell.row, cell.column)
    }

    for {
      row <- cells
      cell <- row
    } listenTo(cell)
  }

  private val rowHeader = new ListView((0 until height).map(_.toString)) {
    fixedCellWidth = 30
    fixedCellHeight = table.rowHeight
  }

  viewportView = table
  rowHeaderView = rowHeader
}