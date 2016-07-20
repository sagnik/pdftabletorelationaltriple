package edu.psu.sagnik.research.table.model

import java.awt.geom.Point2D
import java.io.File

import edu.psu.sagnik.research.pdsimplify.impl.ProcessDocument
import edu.psu.sagnik.research.pdsimplify.path.impl.BB
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

  def isWithinTable(pdSegment:PDSegment,tableBBVals:Seq[Float],pageHeight:Float)={
    val segmentBB=Rectangle(
      pdSegment.bb.x1,
      pageHeight-pdSegment.bb.y1,
      pdSegment.bb.x2,
      pageHeight-pdSegment.bb.y2
    )
    val tableBB=Rectangle(
      tableBBVals(0),
      tableBBVals(1),
      tableBBVals(2),
      tableBBVals(3)
    )
    Rectangle.rectInside(segmentBB,tableBB)
  }

  def transformPDSegment(pdSegment:PDSegment,bb:Seq[Float],pageHeight:Float)=pdSegment match{
    case pdSegment:PDLine =>
      val startPoint=new Point2D.Float(pdSegment.startPoint.x-bb.head,pageHeight-pdSegment.startPoint.y-bb(1))
      val endPoint=new Point2D.Float(pdSegment.endPoint.x-bb.head,pageHeight-pdSegment.endPoint.y-bb(1))
      PDLine(
         startPoint=startPoint,
         endPoint=endPoint,
        bb=BB.Line(startPoint,endPoint)
      )

    case pdSegment:PDCurve =>
      val startPoint=new Point2D.Float(pdSegment.startPoint.x-bb.head,pageHeight-pdSegment.startPoint.y-bb(1))
      val controlPoint1=new Point2D.Float(pdSegment.controlPoint1.x-bb.head,pageHeight-pdSegment.controlPoint1.y-bb(1))
      val controlPoint2=new Point2D.Float(pdSegment.controlPoint2.x-bb.head,pageHeight-pdSegment.controlPoint2.y-bb(1))
      val endPoint=new Point2D.Float(pdSegment.endPoint.x-bb.head,pageHeight-pdSegment.endPoint.y-bb(1))
      PDCurve(
        startPoint=startPoint,
        controlPoint1=controlPoint1,
        controlPoint2=controlPoint2,
        endPoint=endPoint,
        bb=BB.Curve(startPoint,endPoint,controlPoint1,controlPoint2)
      )

  }

  def getPDLines(pdLoc:String,bb:Seq[Float],pageNumber:Int)={
        val pdDoc=PDDocument.load(new File(pdLoc))
        val simplePage=ProcessDocument(pdDoc).pages(pageNumber-1)
        pdDoc.close()
        val (pageHeight,pageWidth)= (simplePage.bb.y2-simplePage.bb.y1,simplePage.bb.x2-simplePage.bb.x1)
        Some(
        for {
          paths <- simplePage.gPaths
          subPaths <- paths.subPaths if !subPaths.fromReCommand
          segments <- subPaths.segments
          if isStraightLine(segments) &&  isWithinTable(segments,bb,pageHeight)
        } yield transformPDSegment(segments,bb,pageHeight)
        )
    }

  def getPageHeightWidth(pdLoc:String,pageNumber:Int)={
    val pdDoc=PDDocument.load(new File(pdLoc))
    val simplePage=ProcessDocument(pdDoc).pages(pageNumber-1)
    pdDoc.close()
    (simplePage.bb.y2-simplePage.bb.y1,simplePage.bb.x2-simplePage.bb.x1)
  }


  def allenAITableToMyTable(atable: AllenAITable, pdLoc: String): Option[IntermediateTable] = {
    (atable.ImageText) match {
      case Some(wordsOrg) =>
        val cvRatio=atable.DPI/72f
        val tableBB = atable.ImageBB.map( _.toFloat / cvRatio.toFloat)
        val words = wordsOrg.map(x=>x.copy(TextBB=x.TextBB.map(_ / cvRatio)))
        val (pageHeight,pageWidth) = getPageHeightWidth(pdLoc,atable.Page)
        println(atable.ImageBB,tableBB)

        val imTable=IntermediateTable(
          bb=Rectangle(tableBB.head, tableBB(1), tableBB(2), tableBB(3)),
          textSegments=words.map(w =>
            A(
              w.Text,
              Rectangle(
                w.TextBB.head - tableBB.head+2, //shortening the table
                w.TextBB(1) - tableBB(1)+2,
                w.TextBB(2) - tableBB.head-2,
                w.TextBB(3) - tableBB(1)-2
              )
            )),

          caption=atable.Caption,
          mention=atable.Mention,
          pageNo=atable.Page,
          pdLines=getPDLines(pdLoc,tableBB,atable.Page), //TODO: Can integrate PDLines that are inside the table later
          pageHeight=pageHeight,
          pageWidth=pageWidth
        )
        if (imTable.textSegments.nonEmpty) Some(imTable)
        else None

      case _ => None
    }
  }


}
