package edu.psu.ist.sagnik.research.table.model

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
  implicit val formats = org.json4s.DefaultFormats

  type A = TextGeneric
  def A(x:String,y:Rectangle)=TextGeneric(x,y)

  def jsonToString(inpfile: String): String = scala.io.Source.fromFile(inpfile).mkString

  def jsonTocaseClasses(jsonstr: String): AllenAITable = parse(jsonstr).extract[AllenAITable] //for test

  def allenAITableToMyTable(atable: AllenAITable): Option[IntermediateTable] = {
    (atable.ImageBB, atable.ImageText, atable.Page) match {
      case (Some(tablebb), Some(words), Some(pageno)) => {
        val imtable=IntermediateTable(
          Rectangle(tablebb(0), tablebb(1), tablebb(2), tablebb(3)),
          words.map(w =>
            A(
              w.Text,
              Rectangle(
                w.TextBB(0) - tablebb(0)+2, //shorteining the table
                w.TextBB(1) - tablebb(1)+2,
                w.TextBB(2) - tablebb(0)-2,
                w.TextBB(3) - tablebb(1)-2
              )
            )),
          atable.Caption,
          atable.Mention,
          pageno,
          None //TODO: Can intgerate PDLines that are inside the table later
        )
        if (imtable.textsegments.nonEmpty) Some(imtable)
        else None
      }
      case _ => None
    }
  }


}
