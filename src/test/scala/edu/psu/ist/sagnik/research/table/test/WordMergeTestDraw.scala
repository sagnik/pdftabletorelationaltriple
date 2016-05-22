package edu.psu.ist.sagnik.research.table.test

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import edu.psu.ist.sagnik.research.table.model.AllenAIDataConversion
import edu.psu.ist.sagnik.research.table.tablecellextraction.CombineWords
import org.scalatest.FunSpec

/**
 * Created by schoudhury on 8/19/15.
 */


class WordMergeTestDraw extends FunSpec{

  def createImage(jsonloc:String):Unit={
    val mytable=AllenAIDataConversion.
      allenAITableToMyTable(
        AllenAIDataConversion.jsonTocaseClasses(
          AllenAIDataConversion.jsonToString(jsonloc
          )
        )
      )
    mytable match{
      case Some(propertable)=>{
        val interimtable=CombineWords.wordMergedTable(propertable)
        //drawing image
        val sourceimage = new File(jsonloc.substring(0,jsonloc.length-5)+".png");
        val original = ImageIO.read(sourceimage);
        val newimage = new BufferedImage(original.getWidth(), original.getHeight(), BufferedImage.TYPE_INT_RGB);
        val graph=newimage.createGraphics()
        graph.drawImage(original,0,0,null)
        graph.setColor(Color.GREEN)
        interimtable.textsegments.foreach(x=>graph.draw(new java.awt.Rectangle(
          x.bb.x1.toInt,x.bb.y1.toInt,(x.bb.x2-x.bb.x1).toInt,(x.bb.y2-x.bb.y1).toInt)))
        graph.dispose()
        ImageIO.write(newimage,"png",new File(jsonloc.substring(0,jsonloc.length-5)+"-mergedwordmedian.png"))
      }
      case None=>{println ("could not merge words in the table");return}
    }

  }

  describe("testing if word merging is correct") {
    it("should print the merged cells from a table") {
      val alljsons=DataLocation.recursiveListFiles(new File("/Users/schoudhury/com-sc-papers/nlp-data/"),"(?=.*Table)(?=.*json)".r)
      //println(alljsons.length)
      alljsons.foreach(x=>{println(x.getAbsolutePath);createImage(x.getAbsolutePath)})
    }
  }
}
