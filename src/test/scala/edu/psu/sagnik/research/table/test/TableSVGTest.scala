package edu.psu.sagnik.research.table.test

import edu.psu.sagnik.research.pdsimplify.path.model.{PDCurve, PDLine, PDSegment, PathStyle}
import edu.psu.sagnik.research.pdsimplify.text.model.PDChar
import edu.psu.sagnik.research.table.model.{AllenAIDataConversion, Rectangle}
import edu.psu.sagnik.research.table.tablecellextraction.{CellRenaming, CombineWords}
import edu.psu.sagnik.research.table.tripleextraction.CriticalCell
import org.scalatest.FunSpec

import scala.reflect.io.File

/**
 * Created by schoudhury on 8/21/15.
 */

class TableSVGTest extends FunSpec {

  def segmentToString(s:PDSegment,h:Float,tableBB:Rectangle):String=s match{
    case s: PDLine =>
      "M " +
        (s.startPoint.x).toString + "," + (h - s.startPoint.y - tableBB.y1).toString +
        " L " +
        (s.endPoint.x).toString + "," + (h - s.endPoint.y -tableBB.y1).toString+
        " "
    case s: PDCurve =>
      "M " +
        (s.startPoint.x - tableBB.x1).toString + "," + (h - s.startPoint.y - tableBB.y1).toString +
        " C " +
        (s.controlPoint1.x - tableBB.x1).toString + "," + (h - s.controlPoint1.y - tableBB.y1).toString + " "+
        (s.controlPoint2.x - tableBB.x1).toString + "," + (h - s.controlPoint2.y - tableBB.y1).toString + " "+
        (s.endPoint.x - tableBB.x1).toString + "," + (h - s.endPoint.y - tableBB.y1).toString+
        " "
    case _ => ""

  }

  val defaultPathStyle="style=\"fill:none;" +
    "stroke:#000000;stroke-width:3.76389003;" +
    "stroke-linecap:butt;stroke-linejoin:miter;" +
    "stroke-miterlimit:10;stroke-dasharray:none;" +
    "stroke-opacity:1\""

  def getPathDString(p:PDSegment,h:Float,tableBB:Rectangle):String={
    val dStringStart=" d=\""
    val segmentStrings=segmentToString(p,h,tableBB)
    val dStringEnd="\""
    dStringStart+segmentStrings+dStringEnd
  }
  def getSvgString[A](p:A,w:Float,h:Float,tableBB:Rectangle):String=p match{
    case p: PDSegment => "<path "+getPathDString(p,h,tableBB)+" "+defaultPathStyle+" />"
    case p: PDChar => ???
  }

  describe("creates an SVG for the table with the paths") {
    it("should print the critical cell from a table") {
      val myTable = AllenAIDataConversion.
        allenAITableToMyTable(
          AllenAIDataConversion.jsonTocaseClasses(
            AllenAIDataConversion.jsonToString(DataLocation.jsonLoc)
          ),DataLocation.pdfLoc
        )

      myTable match {
        case Some(properTable) =>
          val pdLines=properTable.pdLines.getOrElse(Seq.empty[PDSegment])
          val svgWidth=properTable.bb.x2 - properTable.bb.x1
          val svgHeight=properTable.bb.y2 - properTable.bb.y1
          val pageHeight=properTable.pageHeight
          val pageWidth=properTable.pageWidth

          val svgStart="<?xml version=\"1.0\" standalone=\"no\"?>\n\n<svg height=\"" +
            svgHeight.toString +
            "\" width=\"" +
            svgWidth.toString +
            "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">"+
            "\n"

          val content= pdLines.map(x => getSvgString(x,pageWidth,pageHeight,properTable.bb)).foldLeft("")((a, b) => a + "\n" + b) + "\n"

          val svgEnd = "\n</svg>"
          File(DataLocation.svgLoc).writeAll(svgStart + content + svgEnd)

        case None => assert(false)
      }

    }
  }
}
