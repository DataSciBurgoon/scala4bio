package translation
import bio_objects.RNA

/**
 * @author burgoonl
 */

/**
 * This class takes in an RNA object.
 * Then it reads the RNA object in all 3 reading frames.
 * Then it translates each triplet in each reading frame to an amino acid
 */
class Translator(val rna_obj: RNA) {
  /**
   * We start by creating the Hash that will translate the triplets
   * to amino acids
   * 
   * Note: Any codon with an N is set to amino acid X
   * Note: Any stop codon is set to an amino acid Z
   * These mappings are in keeping with BLAT notation.
   */
  
  def translate():Map[Int, String] = {
    val triplet_to_amino_acid = Map("UUU" -> "F",
                                  "UUC" -> "F",
                                  "UUA" -> "L",
                                  "UUG" -> "L",
                                  "UCU" -> "S",
                                  "UCC" -> "S",
                                  "UCA" -> "S",
                                  "UCG" -> "S",
                                  "UAU" -> "Y",
                                  "UAC" -> "Y",
                                  "UAA" -> "Z",
                                  "UAG" -> "Z",
                                  "UGU" -> "C",
                                  "UGC" -> "C",
                                  "UGA" -> "Z",
                                  "UGG" -> "W",
                                  "CUU" -> "L",
                                  "CUC" -> "L",
                                  "CUA" -> "L",
                                  "CUG" -> "L",
                                  "CCU" -> "P",
                                  "CCC" -> "P",
                                  "CCA" -> "P",
                                  "CCG" -> "P",
                                  "CAU" -> "H",
                                  "CAC" -> "H",
                                  "CAA" -> "Q",
                                  "CAG" -> "Q",
                                  "CGU" -> "R",
                                  "CGC" -> "R",
                                  "CGA" -> "R",
                                  "CGG" -> "R",
                                  "AUU" -> "I",
                                  "AUC" -> "I",
                                  "AUA" -> "I",
                                  "AUG" -> "M",
                                  "ACU" -> "T",
                                  "ACC" -> "T",
                                  "ACA" -> "T",
                                  "ACG" -> "T",
                                  "AAU" -> "N",
                                  "AAC" -> "N",
                                  "AAA" -> "K",
                                  "AAG" -> "K",
                                  "AGU" -> "S",
                                  "AGC" -> "S",
                                  "AGA" -> "R",
                                  "AGG" -> "R",
                                  "GUU" -> "V",
                                  "GUC" -> "V",
                                  "GUA" -> "V",
                                  "GUG" -> "V",
                                  "GCU" -> "A",
                                  "GCC" -> "A",
                                  "GCA" -> "A",
                                  "GCG" -> "A",
                                  "GAU" -> "D",
                                  "GAC" -> "D",
                                  "GAA" -> "E",
                                  "GAG" -> "E",
                                  "GGU" -> "G",
                                  "GGC" -> "G",
                                  "GGA" -> "G",
                                  "GGG" -> "G")
    
    val rna_seq = rna_obj.sequence
    var protein_seqs:Map[Int, String] = Map()
    
    for(i <- 0 to 2){
      var rna_seq_temp = rna_seq.drop(i)
      var rna_seq_reading_frames = trim_for_reading_frames(rna_seq_temp)
      var rna_seq_iterator = rna_seq_reading_frames.sliding(3,3)
      var protein_seq = new String
      for(word <- rna_seq_reading_frames.sliding(3,3)){
        var amino_acid = ""
        if(word.contains("N")){
          amino_acid = "X"
        }
        else{
          amino_acid = triplet_to_amino_acid(word)
        }
        protein_seq += amino_acid
      }
      protein_seqs += (i -> protein_seq)
    }
    protein_seqs
  
  }

def trim_for_reading_frames(rna_seq: String): String = {
    if(rna_seq.length() % 3 == 0){
      return(rna_seq)
    }
    else{
      var remainder = rna_seq.length() % 3
      var rna_seq_trimmed = rna_seq.dropRight(remainder)
      return(rna_seq_trimmed)
    }
  }
    
}