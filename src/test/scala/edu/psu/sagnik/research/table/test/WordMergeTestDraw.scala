package edu.psu.sagnik.research.table.test

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import edu.psu.sagnik.research.table.model.AllenAIDataConversion
import edu.psu.sagnik.research.table.tablecellextraction.CombineWords
import org.scalatest.FunSpec

/**
 * Created by schoudhury on 8/19/15.
 */


class WordMergeTestDraw extends FunSpec{

  def createImage(jsonLoc:String):Unit={
    val mytable=AllenAIDataConversion.
      allenAITableToMyTable(
        AllenAIDataConversion.jsonTocaseClasses(
          AllenAIDataConversion.jsonToString(jsonLoc
          )
        )
      )
    mytable match{
      case Some(propertable)=>{
        val interimtable=CombineWords.wordMergedTable(propertable)
        //drawing image
        val sourceimage = new File(jsonLoc.substring(0,jsonLoc.length-5)+".png");
        val original = ImageIO.read(sourceimage);
        val newimage = new BufferedImage(original.getWidth(), original.getHeight(), BufferedImage.TYPE_INT_RGB);
        val graph=newimage.createGraphics()
        graph.drawImage(original,0,0,null)
        graph.setColor(Color.GREEN)
        interimtable.textsegments.foreach(x=>graph.draw(new java.awt.Rectangle(
          x.bb.x1.toInt,x.bb.y1.toInt,(x.bb.x2-x.bb.x1).toInt,(x.bb.y2-x.bb.y1).toInt)))
        graph.dispose()
        ImageIO.write(newimage,"png",new File(jsonLoc.substring(0,jsonLoc.length-5)+"-mergedwordmedian.png"))
      }
      case None=>{println ("could not merge words in the table");return}
    }

  }

  def createImage(jsonLoc:String, imageLoc:String):Unit={
    val mytable=AllenAIDataConversion.
      allenAITableToMyTable(
        AllenAIDataConversion.jsonTocaseClasses(
          AllenAIDataConversion.jsonToString(jsonLoc
          )
        )
      )
    mytable match{
      case Some(properTable)=>{
        val interimTable=CombineWords.wordMergedTable(properTable)
        //drawing image
        val sourceImage = new File(imageLoc);
        val original = ImageIO.read(sourceImage);
        val newimage = new BufferedImage(original.getWidth(), original.getHeight(), BufferedImage.TYPE_INT_RGB);
        val graph=newimage.createGraphics()
        graph.drawImage(original,0,0,null)
        graph.setColor(Color.GREEN)
        interimTable.textsegments.foreach(x=>graph.draw(new java.awt.Rectangle(
          x.bb.x1.toInt,x.bb.y1.toInt,(x.bb.x2-x.bb.x1).toInt,(x.bb.y2-x.bb.y1).toInt)))
        graph.dispose()
        ImageIO.write(newimage,"png",new File(imageLoc.substring(0,imageLoc.length-4)+"-wordmerged.png"))
      }
      case None=>{println ("could not merge words in the table");return}
    }

  }

  describe("testing if word merging is correct") {
    it("should print the merged cells from a table") {
      //val alljsons=DataLocation.recursiveListFiles(new File("/Users/schoudhury/com-sc-papers/nlp-data/"),"(?=.*Table)(?=.*json)".r)
      //println(alljsons.length)
      //alljsons.foreach(x=>{println(x.getAbsolutePath);createImage(x.getAbsolutePath)})
      val jsonLoc=DataLocation.jsonLoc
      val imageLoc=DataLocation.imageLoc
      createImage(jsonLoc,imageLoc)

    }
  }
}
