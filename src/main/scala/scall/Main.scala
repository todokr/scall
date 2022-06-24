package scall

import scala.swing._

object Main extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "scall"
    contents = new SpreadSheet(30, 40)
  }
}