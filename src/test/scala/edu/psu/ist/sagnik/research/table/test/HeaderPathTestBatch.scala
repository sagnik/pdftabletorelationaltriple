package edu.psu.ist.sagnik.research.table.test

import java.io.File

import edu.psu.ist.sagnik.research.table.model.AllenAIDataConversion
import edu.psu.ist.sagnik.research.table.tablecellextraction.{CellRenaming, CombineWords}
import edu.psu.ist.sagnik.research.table.tripleextraction.TabletoWFT
import org.scalatest.FunSpec

/**
 * Created by schoudhury on 8/19/15.
 */


class HeaderPathTestBatch extends FunSpec {

  def deleteFile(path: String) = {
    val fileTemp = new File(path)
    if (fileTemp.exists) {
      fileTemp.delete()
    }
  }

  def RowHeaderPathTest(jsonloc:String): Unit ={
    val mytable = AllenAIDataConversion.
      allenAITableToMyTable(
        AllenAIDataConversion.jsonTocaseClasses(
          AllenAIDataConversion.jsonToString(jsonloc
          )
        )
      )
    implicit val formats = org.json4s.DefaultFormats
    mytable match {
      case Some(propertable) => {
        val interimtable = CombineWords.wordMergedTable(propertable)
        val table = CellRenaming.produceRowColNumbers(interimtable)
        if(table.cells.length==interimtable.textsegments.length) {
          TabletoWFT.headerPathstoDataCells(table) match {
            case Some(wft) => {
              scala.tools.nsc.io.File(jsonloc.substring(0,jsonloc.length-5)+"-wft.json")
                .writeAll(JSONFormatter.wftToJsonString(wft))
            }
            case None => println("Could not convert given table to a well formed table")
          }
        }
        else println("Failed to generate row col header from intermediate table")
      }
      case None => println("Failed to generate AllenAI table")
    }

  }
  describe("testing if row column prediction is correct") {
    it("should print the rows and cols from a table") {
      //first delete all existing wft jsons.
      DataLocation.recursiveListFiles(new File("/home/sagnik/data/nlp-table-data/jsonsfortripleextraction-dir/"),"(?=.*Table)(?=.*wft)(?=.*json)".r)
        .foreach(x=>deleteFile(x.getAbsolutePath))

      DataLocation.recursiveListFiles(new File("/home/sagnik/data/nlp-table-data/jsonsfortripleextraction-dir/"),"(?=.*Table)(?=.*json)".r)
      .foreach(x=>{println(x.getAbsolutePath);RowHeaderPathTest(x.getAbsolutePath)})
    }
  }
}
