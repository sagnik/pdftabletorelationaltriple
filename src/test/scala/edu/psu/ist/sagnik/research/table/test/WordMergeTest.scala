package edu.psu.ist.sagnik.research.table.test

import edu.psu.ist.sagnik.research.table.tablecellextraction.CombineWords
import org.scalatest.FunSpec
import edu.psu.ist.sagnik.research.table.model.AllenAIDataConversion

/**
 * Created by schoudhury on 8/19/15.
 */

class WordMergeTest extends FunSpec{
  describe("testing if word merging is correct") {
    it("should print the merged cells from a table") {
      val mytable=AllenAIDataConversion.
        allenAITableToMyTable(
          AllenAIDataConversion.jsonTocaseClasses(
            AllenAIDataConversion.jsonToString(DataLocation.jsonloc
            )
          )
        )
      mytable match{
        case Some(propertable)=>{
          val interimtable=CombineWords.wordMergedTable(propertable)
          println(s"caption: ${interimtable.caption}")
          println(s"mention: ${interimtable.mention}")
          println(s"bounding box: ${interimtable.bb}")
          interimtable.textsegments.foreach(x=>println(s"merged words: ${x.content} bb: ${x.bb}"))
        }
        case None=>assert(false)
      }

    }
  }
}
