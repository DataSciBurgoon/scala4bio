package transcription

import bio_objects.DNA
import bio_objects.RNA

/**
 * @author burgoonl
 */
class Transcriber(var cdna: DNA) {
  def transcribe(): String = {
    /*val dna_to_rna_map = Map("A" -> "U",
                             "C" -> "G",
                             "G" -> "C",
                             "T" -> "A")*/
    val rna_seq = cdna.sequence map {case 'A' => 'U' 
                                     case 'C' => 'G'
                                     case 'G' => 'C'
                                     case 'T' => 'A'
                                     case 'N' => 'N'}
    
    return(rna_seq)
  }
  
}