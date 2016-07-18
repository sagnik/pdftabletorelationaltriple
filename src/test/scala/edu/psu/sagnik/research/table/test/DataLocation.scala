package edu.psu.sagnik.research.table.test

import java.io.File

/**
 * Created by schoudhury on 8/21/15.
 */
object DataLocation {
  //val jsonLoc = "src/test/resources/jsons/10.1.1.10.1035-Table-2.json"
  //val jsonLoc = "src/test/resources/jsons/10.1.1.194.433-Table-2.json"
  //val jsonLoc = "src/test/resources/jsons/10.1.1.106.5870-Table-4.json"
  //val jsonLoc = "src/test/resources/jsons/10.1.1.159.3090-Table-7.json"
  //val jsonLoc = "src/test/resources/jsons/N10-1104-Table-1.json"
  val b="N10-1104-Table-1"
  val baseDir="src/test/resources/jsons/"
  val jsonLoc=baseDir+b+".json"
  val imageLoc=baseDir+b+".png"

  import scala.util.matching.Regex
  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,r))
  }
}
