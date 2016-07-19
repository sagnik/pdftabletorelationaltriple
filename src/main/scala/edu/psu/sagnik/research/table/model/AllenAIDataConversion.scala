package edu.psu.sagnik.research.table.model

import java.io.File

import edu.psu.sagnik.research.pdsimplify.impl.ProcessDocument
import edu.psu.sagnik.research.pdsimplify.path.impl.ProcessPaths
import org.apache.pdfbox.pdmodel.PDDocument
import edu.psu.sagnik.research.pdsimplify.path.model._
import org.json4s.native.JsonMethods._

/**
 * Created by schoudhury on 8/21/15.
 */
/*
We are currently using the output from pdffigures code from allenai (https://github.com/allenai/pdffigures)
This does a really good job in producing table boundaries in scholarly papers. The case classes
in TableADT.scala show the JSON schema.
If you change that to Roman's output, you have to first figure out a way to write
or produce a JSON file with the case class schema given in com.nitro.research.model.TableADT.AllenAITable.
*/

object AllenAIDataConversion {


  protected implicit class FloatEquality(val a: Float) extends AnyVal {
    @inline def isEqualFloat(b: Float): Boolean =
      math.abs(a - b) <= 0.00001
  }

  implicit val formats = org.json4s.DefaultFormats

  type A = TextGeneric
  def A(x:String,y:Rectangle)=TextGeneric(x,y)

  def jsonToString(inpFile: String): String = scala.io.Source.fromFile(inpFile).mkString

  def jsonTocaseClasses(jsonStr: String): AllenAITable = parse(jsonStr).extract[AllenAITable] //for test

  def isStraightLine(s:PDSegment)=s match{
    case s:PDLine => (s.startPoint.x isEqualFloat s.endPoint.x) || (s.startPoint.y isEqualFloat s.endPoint.y) //a horizontal or a vertical line
    case s:PDCurve =>
      (s.startPoint.x isEqualFloat s.controlPoint1.x) &&  (s.controlPoint1.x isEqualFloat s.controlPoint2.x) && (s.controlPoint2.x isEqualFloat s.endPoint.x)||
        (s.startPoint.y isEqualFloat s.controlPoint1.y) &&  (s.controlPoint1.y isEqualFloat s.controlPoint2.y) && (s.controlPoint2.y isEqualFloat s.endPoint.y)
    //A Beizer curve can be a straight line when the start, end and the control points are in the same line.
    // Among them, we only want the ones that are horizontal or vertical.
    case _ => false
  }

  def isWithinTable(pdSegment:PDSegment,tableBBVals:Seq[Int],cvRatio:Float,pageHeight:Float)={
    val segmentBB=Rectangle(
      pdSegment.bb.x1,
      pageHeight-pdSegment.bb.y1,
      pdSegment.bb.x2,
      pageHeight-pdSegment.bb.y2
    )
    val tableBB=Rectangle(
      tableBBVals(0)/cvRatio,
      tableBBVals(1)/cvRatio,
      tableBBVals(2)/cvRatio,
      tableBBVals(3)/cvRatio
    )
    Rectangle.rectInside(segmentBB,tableBB)
  }

  def getPDLines(pdLoc:String,bb:Seq[Int],pageNumber:Int, DPI:Int)={
        //TODO:fix in PDSimplify the document closing thing.
        val simplePage=ProcessDocument(pdLoc).pages(pageNumber-1) //TODO: check if the page number in AllenAI starts with 0.
        val pageHeight = simplePage.bb.y2-simplePage.bb.y1
        val imageConversionRatio=DPI/72f
        Some(
        for {
          paths <- simplePage.gPaths
          subPaths <- paths.subPaths if !subPaths.fromReCommand
          segments <- subPaths.segments
          if isStraightLine(segments) &&  isWithinTable(segments,bb,imageConversionRatio,pageHeight)
        } yield segments
        )
    }

  def allenAITableToMyTable(atable: AllenAITable, pdLoc: String): Option[IntermediateTable] = {
    (atable.ImageBB, atable.ImageText, atable.Page) match {
      case (tablebb, Some(words), pageno) =>
        val imtable=IntermediateTable(
          bb=Rectangle(tablebb(0), tablebb(1), tablebb(2), tablebb(3)),
          textSegments=words.map(w =>
            A(
              w.Text,
              Rectangle(
                w.TextBB(0) - tablebb(0)+2, //shorteining the table
                w.TextBB(1) - tablebb(1)+2,
                w.TextBB(2) - tablebb(0)-2,
                w.TextBB(3) - tablebb(1)-2
              )
            )),
          caption=atable.Caption,
          mention=atable.Mention,
          pageNo=pageno,
          pdLines=getPDLines(pdLoc,atable.ImageBB,atable.Page,atable.DPI), //TODO: Can integrate PDLines that are inside the table later
          pageHeight = atable.Height,
          pageWidth = atable.Width
        )
        if (imtable.textSegments.nonEmpty) Some(imtable)
        else None

      case _ => None
    }
  }


}
