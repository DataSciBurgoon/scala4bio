package fasta

import scala.collection.mutable.ArrayBuffer
import bio_objects.Protein
import java.io._

/**
 * @author burgoonl
 */
class FastaWriter(val proteins: ArrayBuffer[Protein], val file_path: String) {
  def file_writer(){
    val file = new File(file_path)
    val buffered_writer = new BufferedWriter(new FileWriter(file))
    for(protein <- proteins){
      buffered_writer.write(">" + protein.annotation_string + "\n")
      buffered_writer.write(protein.sequence + "\n")
    }
    buffered_writer.close()
  }
}