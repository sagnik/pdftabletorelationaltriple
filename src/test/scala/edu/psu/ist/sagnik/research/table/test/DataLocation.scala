package edu.psu.ist.sagnik.research.table.test

import java.io.File

/**
 * Created by schoudhury on 8/21/15.
 */
object DataLocation {
    //val jsonloc = "src/test/resources/jsons/10.1.1.10.1035-Table-2.json"
    //val jsonloc = "src/test/resources/jsons/10.1.1.194.433-Table-2.json"
    //val jsonloc = "src/test/resources/jsons/10.1.1.106.5870-Table-4.json"
    //val jsonloc = "src/test/resources/jsons/10.1.1.159.3090-Table-7.json"
    //val jsonloc="src/test/resources/jsons/N10-1104-Table-1.json"
    val jsonloc="/home/sagnik/data/nlp-table-data/jsonsfortripleextraction-dir/C10-1112-Table-4.json"

    val imageloc="/home/sagnik/data/nlp-table-data/randpngs/C10-1112-Table-4.png"
    import scala.util.matching.Regex
    def recursiveListFiles(f: File, r: Regex): Array[File] = {
        val these = f.listFiles
        val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
        good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,r))
    }
  }
