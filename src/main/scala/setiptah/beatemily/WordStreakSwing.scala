package setiptah.beatemily

import java.awt.{FlowLayout, BorderLayout}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.GroupLayout.Alignment
import javax.swing._
import javax.swing.SwingUtilities

import scala.collection.mutable

/**
  * Created by ktreleaven on 2/3/2016.
  */
object WordStreakSwing extends App {

  override def main(args: Array[String]) : Unit = {

    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val mainFrame = new WordStreakFrame
      }
    })

  }

}

class WordStreakFrame extends JFrame("Word Streak") {

  import WordStreakApp._

  setLayout(new BorderLayout())

  val demoBoard = getLetterBoard("SSFLESONWTNDHAIE", 4)

  val demoWords = getFileWords("wordsEn.txt")

  val dictionary = DictionaryTree.fromWords(demoWords)

  val found = WordStreakAlgorithm.findAllWords(demoBoard)(dictionary)

  val words = found.values.flatten.filter( w => w.length >= 4 ).iterator

  val label = new JLabel("")
  val oldFont = label.getFont
  val bigFont = oldFont.deriveFont(20)
  label.setFont(bigFont)

  def getMore[T](iter: Iterator[T], n: Int) : List[T] = {
    val ret = mutable.ListBuffer.empty[T]

    for ( k <- 0 until n ) {
      if (iter.hasNext) {
        ret += iter.next()
      }
    }

    return ret.toList
  }

  def setNextLabel : Unit = {
    if (words.hasNext) {

      val nextWords = getMore(words, 4)
      val nextLabelText = nextWords.mkString(" \n")

      label.setText( nextLabelText )
    }

    else {
      label.setText("")
    }
  }

  val btn = new JButton("Next")

  getRootPane.setDefaultButton(btn)

  add(label,BorderLayout.CENTER)
  add(btn,BorderLayout.SOUTH)

  btn.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {

      setNextLabel

    }
  })

  setNextLabel

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setSize(400,300)
  setVisible(true)
}