package edu.psu.ist.sagnik.research.table.tablecellextraction

/**
 * Created by schoudhury on 8/13/15.
 */
/*
we will take a JSON file that contains table boundary and the bounding boxes
of the words inside it. Words will be horizontally combined to form what a cell
content. Multiiline cells are not considered at this point. (It would be awesome to
combine them vertically as well, but that seems hard at this moment).

We are currently using the output from pdffigures code from allenai (https://github.com/allenai/pdffigures)
This does a really good job in producing table boundaries in scholarly papers. The case classes
in TableADT.scala show the JSON schema.
If you change that to Roman's output, you have to first figure out a way to write
or produce a JSON file with the case class schema in model.IntermediateTable. For an example,
see model.AllenAIDataConversion
*/

import edu.psu.ist.sagnik.research.table.model.{IntermediateTable, Rectangle, TextGeneric}



object CombineWords {

  //this will reduce the pain in writing TextGeneric so many times :)
  type A = TextGeneric
  def A(x:String,y:Rectangle)=TextGeneric(x,y)

  def wordMergedTable(table: IntermediateTable): IntermediateTable = table.copy(
    textsegments = horizontalMerge(table.textsegments,table.pdlines,
    WordMergeHeuristics.mergeThresholdWordMedian))

  /* this function will be changed with linear chain CRFs to facilitate merging*/
  def horizontalMerge(words: Seq[A], pdlines: Option[Seq[Int]],f:Seq[A]=>Float):Seq[A] = {
    val threshold=f(words)
    println(s"The horizontal distance threshold for merging words is ${threshold}")
    merge(words, Nil, threshold)
  }


  def mergedWord(x:A,y:A):A={
    val (left,right)=if (y.bb.x2<x.bb.x1) (y,x) else (x,y)
    A(left.content + " " + right.content,
      Rectangle(left.bb.x1, List(x.bb.y1, y.bb.y1).min, right.bb.x2, List(x.bb.y2, y.bb.y2).max))
  }

  def shouldBeMerged(x:A,y:A,threshold:Float):Boolean={
    val (left,right)=if (y.bb.x2<x.bb.x1) (y,x) else (x,y)
    isSubSuperscript(left,right)||(
    right.bb.x1 - left.bb.x2 < threshold &&
      scala.math.abs(y.bb.y1 - x.bb.y1) <= 2) //to ensure that the merged words are from the same vertical line
  }

  def isSubSuperscript(left:A,right:A):Boolean=(right.bb.y2-right.bb.y1)<0.75*(left.bb.y2-left.bb.y1)&&
    right.bb.x1-left.bb.x2<2 && right.bb.x1-left.bb.x2>(-1) && //very close horizontally but to the right
    (left.bb.y2-right.bb.y1>0 || left.bb.y1-right.bb.y2>0) //subscript or superscrpt


  def getMergingElement(x:A,words:Seq[A],threshold:Float):Option[(A,A)]={
    val sortedwords = words.sortWith(_.bb.x1 < _.bb.x1)
    val word=sortedwords.find(y=>x!=y && shouldBeMerged(x,y,threshold))
    word match{
      case Some(matchedword)=>Some((mergedWord(x,matchedword),matchedword))
      case None=>None
    }
  }

  /* All the elements in the list are unique. Will check if there is a merging possibility with the
   elements, and choose the one that is horizontally closest.
    */

  def merge(l: Seq[A], accum: Seq[A], threshold: Float): Seq[A] = {
    l match {

      case x :: ys => {
        val tobemerged = getMergingElement(x,l,threshold)
        tobemerged match {
          case Some((merged,toberemoved)) => merge(ys.filterNot(word=>word==toberemoved):+merged, accum, threshold)
          case None => {
            merge(ys, (accum :+ x), threshold)
          }
        }
      }
      case Nil => accum
    }
  }


}
