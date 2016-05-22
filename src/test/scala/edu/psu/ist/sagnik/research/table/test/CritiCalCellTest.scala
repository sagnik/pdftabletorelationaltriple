package edu.psu.ist.sagnik.research.table.test
import edu.psu.ist.sagnik.research.table.tablecellextraction.{CellRenaming, CombineWords}
import edu.psu.ist.sagnik.research.table.tripleextraction.CriticalCell
import org.scalatest.FunSpec
import edu.psu.ist.sagnik.research.table.model.AllenAIDataConversion

/**
 * Created by schoudhury on 8/21/15.
 */

class CritiCalCellTest extends FunSpec {
  describe("testing if we can find the critical cell for a table") {
    it("should print the critical cell from a table") {
      val mytable = AllenAIDataConversion.
        allenAITableToMyTable(
          AllenAIDataConversion.jsonTocaseClasses(
            AllenAIDataConversion.jsonToString(DataLocation.jsonloc
            )
          )
        )
      mytable match {
        case Some(propertable) => {
          val interimtable = CombineWords.wordMergedTable(propertable)
          val table = CellRenaming.produceRowColNumbers(interimtable)
          println(s"caption: ${table.caption}")
          println(s"mention: ${table.context}")
          println(s"bounding box: ${table.bb}")
          table.cells.foreach(x => println(s"cell: startrow: ${x.startRow}, startcol: ${x.startCol}" +
            s"content: ${x.tg.content}"))
          //assert(table.cells.length == interimtable.textsegments.length)
          CriticalCell.getCriticalCell(table) match {
            case Some(criticalcell)=> println (s"the critical cell for the table is\n" +
              s"cell content: ${criticalcell.tg.content} startrow: ${criticalcell.startRow} startcol: ${criticalcell.startCol}")
            case None=>println("No critical cell found!!!")
          }
        }
        case None => assert(false)
      }

    }
  }
}
