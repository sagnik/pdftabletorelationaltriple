package edu.psu.ist.sagnik.research.table.test

import edu.psu.ist.sagnik.research.table.model.AllenAIDataConversion
import edu.psu.ist.sagnik.research.table.tablecellextraction.{CellRenaming, CombineWords}
import edu.psu.ist.sagnik.research.table.tripleextraction.TabletoWFT
import org.scalatest.FunSpec

/**
 * Created by schoudhury on 8/19/15.
 */
class HeaderPathTest extends FunSpec {

   describe("testing if row column prediction is correct") {
    it("should print the rows and cols from a table") {
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
          assert(table.cells.length==interimtable.textsegments.length)
          TabletoWFT.headerPathstoDataCells(table) match {
            case Some(wft)=> {
              /*scala.tools.nsc.io.File(DataLocation.jsonloc.split(".json")(0)+"-wft.json")
                .writeAll(JSONFormatter.wftToJsonString(wft))*/

              println("\n-----------------\ncaption\n-----------------\n")
              println(wft.caption)
              println("\n-----------------\nmention\n-----------------\n")
              println(wft.context)
              println("\n-----------------\ntable content\n-----------------\n")
              println(wft.content)
              println("\n-----------------\nrow header cells\n-----------------\n")
              wft.rhcs.foreach(x=>println(s"${x.tg.content}"))
              println("\n-----------------\ncol header cells\n-----------------\n")
              wft.chcs.foreach(x=>println(s"${x.tg.content}"))
              println("\n-----------------\ndata cells\n-----------------\n")
              wft.dcs.foreach(x=>println(s"${x.tg.content} rowpath: ${x.rowpath.map(a=>a.tg.content)}" +
                s"colpath: ${x.colpath.map(a=>a.tg.content)}"))
              println(DataLocation.jsonloc.substring(0,DataLocation.jsonloc.length-5)+"-wft.json")
              scala.tools.nsc.io.File(DataLocation.jsonloc.substring(0,DataLocation.jsonloc.length-5)+"-wft.json")
                .writeAll(JSONFormatter.wftToJsonString(wft))
            }
            case None=>println("Could not convert given table to a well formed table")
          }
        }
        case None => assert(false)
      }

    }
  }
}
