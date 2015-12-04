package tests

import org.junit.Test
import org.junit.Assert._
import bio_objects.Protein
import bio_objects.RNA
import translation.Translator

/**
 * @author burgoonl
 */
class TestTranslator {
  @Test def translation(){
    val rna_seq = "ACCUCGGACGAUUUCACAGAGAUCUACGGGCGAGCA"
    val rna_obj = new RNA("name1", rna_seq)
    var protein_translator = new Translator(rna_obj)
    var protein_seqs = protein_translator.translate()
    assertEquals(protein_seqs(0), "TSDDFTEIYGRA")
    assertEquals(protein_seqs(1), "PRTISQRSTGE")
    assertEquals(protein_seqs(2), "LGRFHRDLRAS")
  }
}